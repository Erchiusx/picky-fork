{- | Scoped asynchronous forks. Every worker is joined or cancelled before its
scope exits, including when the parent fails or is asynchronously cancelled.
-}
module Agent.PickyFork.Agentic.Subagent
  ( withSubagent
  , joinSubagent
  ) where

import Agent.PickyFork.Agentic.Runner
import Agent.PickyFork.Agentic.Types
import Agent.PickyFork.Llm (LlmHistory)
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS.Strict (RWST (..), ask, get, modify, tell)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

{- | The parent selects context per fork. The child receives only that context,
starts with isolated state, and runs at the target node. Budgets are per run.
The continuation can do other work before calling joinSubagent.
-}
withSubagent
  :: ForkId
  -> AgentRunId
  -> HistorySelectionPolicy
  -> ForkTarget
  -> Agentic LlmHistory
  -> (AgentRunId -> Agentic a)
  -> Agentic a
withSubagent forkId childId policy target task continuation = do
  env <- ask
  state <- get
  when
    ( childId == identityRunId (envIdentity env)
        || Map.member childId (stateRunningSubagents state)
    )
    (throwError (AgentGraphError "subagent run ID is already in use"))
  let budget = stateBudget state
  when
    (maybe False (budgetUsedSubagents budget >=) (budgetMaxSubagents budget))
    (throwError (AgentBudgetExceeded "subagents limit exceeded"))
  request <- buildForkRequestWith policy forkId target
  let
    node = AgentNodePointer (forkTargetNode target) (forkTargetAttempt target)
    childEnv =
      env
        { envIdentity =
            AgentIdentity
              (AgentId (unAgentRunId childId))
              childId
              (Just (identityRunId (envIdentity env)))
        , envCurrentNode = node
        , envProvidedHistory = forkRequestSelectedHistory request
        , -- The parent has already selected the exact prefix. Do not expand it.
          envHistorySelectionPolicy =
            SelectExplicitNodes (map segmentNode (forkRequestSelectedHistory request))
        }
    childState =
      initialAgentState
        { stateActiveTools = envToolRegistry env
        , stateBudget =
            budget{budgetUsedLlmCalls = 0, budgetUsedToolCalls = 0, budgetUsedSubagents = 0}
        }
    childAction = do
      tellEvent (AgentStarted (envIdentity childEnv))
      result <- executeNodeTask node task
      tellEvent (AgentFinished childId)
      pure result
    runChild =
      fmap
        ( fmap
            ( \(result, childFinal, events) ->
                ( result
                    { runResultHistory = unHistoryTrace (stateHistoryTrace childFinal)
                    , runResultArtifacts = stateProducedArtifacts childFinal
                    }
                , events
                )
            )
        )
        (runAgentic childEnv childState childAction)
  Agentic $ RWST $ \parentEnv parentState -> ExceptT $
    withAsync runChild $ \handle -> runAgentic parentEnv parentState $ do
      modify
        ( \s ->
            s
              { stateRunningSubagents =
                  Map.insert childId (RunningSubagent request handle) (stateRunningSubagents s)
              , stateNodeStatuses =
                  Map.insert (pointerNode node) (NodeRunning childId) (stateNodeStatuses s)
              , stateBudget = budget{budgetUsedSubagents = budgetUsedSubagents budget + 1}
              }
        )
      tellEvent (SubagentForked request)
      tellEvent
        (BudgetUpdated budget{budgetUsedSubagents = budgetUsedSubagents budget + 1})
      value <- continuation childId
      -- Joining twice is an error, but an explicit join in the continuation is
      -- optional: scope exit always collects an unjoined successful worker.
      running <- Map.member childId . stateRunningSubagents <$> get
      when running (joinSubagent childId >> pure ())
      pure value

joinSubagent :: AgentRunId -> Agentic AgentRunResult
joinSubagent childId = do
  state <- get
  running <-
    maybe
      (throwError (AgentGraphError "unknown or already joined subagent"))
      pure
      (Map.lookup childId (stateRunningSubagents state))
  outcome <- liftIO (waitCatch (runningSubagentHandle running))
  result <- case outcome of
    Left exc -> failChild (AgentSubagentFailed childId (Text.pack (show exc)))
    Right (Left err) -> failChild err
    Right (Right (value, events)) -> tell events >> pure value
  mapM_ appendHistorySegment (runResultHistory result)
  modify
    ( \s ->
        s
          { stateRunningSubagents = Map.delete childId (stateRunningSubagents s)
          , stateNodeStatuses =
              Map.insert
                (pointerNode (runResultNode result))
                NodeCompleted
                (stateNodeStatuses s)
          , stateProducedArtifacts = stateProducedArtifacts s <> runResultArtifacts result
          }
    )
  tellEvent (SubagentFinished childId result)
  tellEvent (SubagentJoined childId)
  pure result
 where
  failChild err = do
    tellEvent (SubagentFailed childId err)
    throwError err
