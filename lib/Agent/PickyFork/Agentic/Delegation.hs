{- | Model-facing delegation: the master lists node histories, then explicitly
selects their keys when it delegates a task. Selection is never expanded.
-}
module Agent.PickyFork.Agentic.Delegation
  ( DelegationTarget (..)
  , delegationTools
  , runDelegationTool
  , historyKey
  ) where

import Agent.PickyFork.Agentic.Loop
import Agent.PickyFork.Agentic.Runner
import Agent.PickyFork.Agentic.Subagent
import Agent.PickyFork.Agentic.Types
import Agent.PickyFork.Llm hiding (ToolCallFailed)
import Control.Monad.RWS.Strict (ask)
import Data.Aeson (Value (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, parseEither)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text

data DelegationTarget = DelegationTarget
  { delegationName :: Text
  , delegationDescription :: Text
  , delegationTarget :: ForkTarget
  }
  deriving stock (Eq, Show)

historyKey :: AgentNodePointer -> Text
historyKey = Text.pack . show

delegationTools :: [DelegationTarget] -> [ToolSpec]
delegationTools targets =
  [ FunctionTool
      (ToolName "list_context")
      "List available node-owned histories and stable keys. Inspect relevance before delegating."
      (objectSchema [] [])
  , FunctionTool
      (ToolName "delegate_task")
      "Run a child at a target node with ONLY the explicitly selected history keys. Supply its task; use an empty history array for no inherited context. Returns after the child finishes."
      ( objectSchema
          [ "target"
              .= object ["type" .= String "string", "enum" .= map delegationName targets]
          , "task" .= object ["type" .= String "string"]
          , "history"
              .= object ["type" .= String "array", "items" .= object ["type" .= String "string"]]
          ]
          ["target", "task", "history"]
      )
  ]
 where
  objectSchema properties required =
    object
      [ "type" .= String "object"
      , "properties" .= object properties
      , "required" .= (required :: [Text])
      , "additionalProperties" .= False
      ]

{- | Install in the master's loopRunTool callback. The supplied loop config is
for children; the host controls which tools children can use.
-}
runDelegationTool
  :: AgentLoopConfig
  -> [DelegationTarget]
  -> ToolCall
  -> Agentic (Either Text ToolOutput)
runDelegationTool childConfig targets call = do
  histories <- availableHistory
  let output value =
        Right
          ( ToolOutput
              (toolCallId call)
              (Just (toolCallName call))
              [ToolOutputJson value]
              (Just True)
          )
  case unToolName (toolCallName call) of
    "list_context" ->
      pure
        ( output
            ( object
                [ "targets"
                    .= [ object
                           ["name" .= delegationName target, "description" .= delegationDescription target]
                       | target <- targets
                       ]
                , "histories"
                    .= [ object
                           [ "key" .= historyKey (segmentNode segment)
                           , "summary" .= segmentSummary segment
                           , "history" .= segmentHistory segment
                           ]
                       | segment <- histories
                       ]
                ]
            )
        )
    "delegate_task" -> case parseEither parseArguments (toolCallArguments call) of
      Left err -> pure (Left (Text.pack err))
      Right (name, task, keys) -> case find ((== name) . delegationName) targets of
        Nothing -> pure (Left "unknown delegation target")
        Just target -> do
          let
            selected =
              [ segmentNode segment
              | segment <- histories
              , historyKey (segmentNode segment) `elem` keys
              ]
            knownKeys = map (historyKey . segmentNode) histories
          if any (`notElem` knownKeys) keys
            then pure (Left "unknown history key; call list_context again")
            else do
              env <- ask
              let
                childId =
                  AgentRunId
                    ( unAgentRunId (identityRunId (envIdentity env))
                        <> ":"
                        <> unToolCallId (toolCallId call)
                    )
                message = LlmMessage Nothing UserRole [TextPart task] Nothing mempty
              result <-
                withSubagent
                  (ForkId (unToolCallId (toolCallId call)))
                  childId
                  (SelectExplicitNodes selected)
                  (delegationTarget target)
                  (runAgentLoop childConfig [InputMessage message])
                  joinSubagent
              pure
                ( output
                    ( object
                        [ "run_id" .= unAgentRunId (runResultAgent result)
                        , "history" .= map segmentHistory (runResultHistory result)
                        ]
                    )
                )
    _ -> pure (Left "unknown delegation tool")
 where
  parseArguments :: Value -> Parser (Text, Text, [Text])
  parseArguments = withObject "delegate_task arguments" $ \o ->
    (,,) <$> o .: "target" <*> o .: "task" <*> o .: "history"
