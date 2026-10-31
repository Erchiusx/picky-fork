-- | A bounded model/tool loop. Tool effects are supplied by the caller.
module Agent.PickyFork.Agentic.Loop
  ( AgentLoopConfig (..)
  , runAgentLoop
  ) where

import Agent.PickyFork.Agentic.Types
import Agent.PickyFork.Llm hiding (ToolCallFailed)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , tryJust
  )
import Control.Monad (forM, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS.Strict (ask, get, modify)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

data AgentLoopConfig = AgentLoopConfig
  { loopRequestTemplate :: LlmRequest
  , loopMaxTurns :: Int
  , loopRunModel :: LlmRequest -> IO (Either LlmError LlmResponse)
  , loopRunTool :: ToolCall -> Agentic (Either Text ToolOutput)
  }

{- | Uses stateOwnHistory as inherited context; returns only this node's delta.
Request-template inputs are per-node instructions, after the inherited prefix.
-}
runAgentLoop :: AgentLoopConfig -> [LlmInputItem] -> Agentic LlmHistory
runAgentLoop config prompt = do
  inherited <- historyInputItems . stateOwnHistory <$> get
  let
    template = loopRequestTemplate config
    ownInput = requestInput template <> prompt
  final <- go (loopMaxTurns config) (inherited <> ownInput)
  pure (LlmHistory (drop (length inherited) final) [])
 where
  go remaining items
    | remaining <= 0 =
        throwError (AgentBudgetExceeded "agent loop turn limit exceeded")
    | otherwise = do
        env <- ask
        state <- get
        let
          budget = stateBudget state
          runId = identityRunId (envIdentity env)
          request =
            (loopRequestTemplate config)
              { requestInput = items
              , requestModel =
                  maybe (requestModel (loopRequestTemplate config)) id (envModel env)
              }
        checkLimit "LLM calls" (budgetMaxLlmCalls budget) (budgetUsedLlmCalls budget)
        updateBudget budget{budgetUsedLlmCalls = budgetUsedLlmCalls budget + 1}
        modify
          ( \s -> s{stateLastLlmRequest = Just request, stateOwnHistory = LlmHistory items []}
          )
        tellEvent (LlmRequestStarted runId request)
        response <-
          safeIO (loopRunModel config request) >>= \case
            Left err -> do
              let failure = AgentLlmError (Text.pack (show err))
              tellEvent (LlmCallFailed runId failure)
              throwError failure
            Right value -> pure value
        modify (\s -> s{stateLastLlmResponse = Just response})
        tellEvent (LlmResponseReceived runId response)
        let
          next = items <> map InputResponse (responseItems response)
          calls = [call | ResponseToolCall call <- responseItems response]
        when
          (null (responseItems response))
          (throwError (AgentLlmError "empty model response"))
        unless
          (length calls == Map.size (Map.fromList [(toolCallId call, ()) | call <- calls]))
          (throwError (AgentToolError "duplicate tool call IDs in model response"))
        modify (\s -> s{stateOwnHistory = LlmHistory next []})
        if null calls
          then pure next
          else do
            -- Reserve a whole batch before any tool side effects.
            currentBudget <- stateBudget <$> get
            case budgetMaxToolCalls currentBudget of
              Just limit
                | budgetUsedToolCalls currentBudget + length calls > limit ->
                    throwError (AgentBudgetExceeded "tool calls limit exceeded")
              _ -> pure ()
            outputs <- forM calls $ \call -> do
              b <- stateBudget <$> get
              updateBudget b{budgetUsedToolCalls = budgetUsedToolCalls b + 1}
              modify
                ( \s ->
                    s
                      { stateActiveToolCalls =
                          Map.insert (toolCallId call) call (stateActiveToolCalls s)
                      }
                )
              tellEvent (ToolCallStarted call)
              output <-
                loopRunTool config call >>= \case
                  Left err -> do
                    tellEvent (ToolCallFailed (toolCallId call) err)
                    pure
                      ( ToolOutput
                          (toolCallId call)
                          (Just (toolCallName call))
                          [ToolOutputText err]
                          (Just False)
                      )
                  Right value -> pure value
              unless
                (toolOutputCallId output == toolCallId call)
                (throwError (AgentToolError "tool output call ID does not match request"))
              let namedOutput = output{toolOutputName = Just (toolCallName call)}
              modify
                ( \s ->
                    s
                      { stateActiveToolCalls = Map.delete (toolCallId call) (stateActiveToolCalls s)
                      , stateCompletedToolOutputs =
                          Map.insert (toolCallId call) namedOutput (stateCompletedToolOutputs s)
                      , stateOwnHistory =
                          (stateOwnHistory s)
                            { historyInputs =
                                historyInputItems (stateOwnHistory s) <> [InputToolOutput namedOutput]
                            , historyResponses = []
                            }
                      }
                )
              tellEvent (ToolCallFinished namedOutput)
              pure (InputToolOutput namedOutput)
            go (remaining - 1) (next <> outputs)

checkLimit :: Text -> Maybe Int -> Int -> Agentic ()
checkLimit label limit used =
  when
    (maybe False (used >=) limit)
    (throwError (AgentBudgetExceeded (label <> " limit exceeded")))

updateBudget :: BudgetState -> Agentic ()
updateBudget budget = do
  modify (\s -> s{stateBudget = budget})
  tellEvent (BudgetUpdated budget)

-- Preserve asynchronous cancellation; turn ordinary transport exceptions into
-- an explicit agent failure rather than leaving a worker running silently.
safeIO :: IO a -> Agentic a
safeIO action = do
  result <- liftIO (tryJust synchronous action)
  either (throwError . AgentLlmError . Text.pack . show) pure result
 where
  synchronous :: SomeException -> Maybe SomeException
  synchronous exc = case fromException exc :: Maybe SomeAsyncException of
    Just _ -> Nothing
    Nothing -> Just exc
