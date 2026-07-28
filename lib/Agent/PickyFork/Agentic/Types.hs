module Agent.PickyFork.Agentic.Types
  ( AgentId (..)
  , AgentRunId (..)
  , AttemptId (..)
  , ForkId (..)
  , ArtifactId (..)
  , CheckpointId (..)
  , AgentIdentity (..)
  , AgentNodePointer (..)
  , HistorySegment (..)
  , HistoryTrace (..)
  , HistorySelectionPolicy (..)
  , ForkTarget (..)
  , ForkRequest (..)
  , RunningSubagent (..)
  , NodeRunStatus (..)
  , EdgeDecision (..)
  , BudgetState (..)
  , AgentEnv (..)
  , AgentState (..)
  , AgentEvent (..)
  , AgentError (..)
  , AgentRunResult (..)
  , Agentic (..)
  , runAgentic
  , evalAgentic
  , execAgentic
  , tellEvent
  )
where

import Agent.PickyFork.Llm
  ( LlmHistory
  , LlmRequest
  , LlmResponse
  , ModelName
  , ToolCall
  , ToolCallId
  , ToolName
  , ToolOutput
  , ToolSpec
  )
import Agent.SSL
  ( GraphNodeRef
  , SkillEdge
  , SkillFlowGraph
  )
import Control.Concurrent.Async (Async)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS.Strict
  ( MonadReader
  , MonadState
  , MonadWriter
  , RWST (runRWST)
  , tell
  )
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype AgentId = AgentId {unAgentId :: Text}
  deriving stock (Eq, Ord, Show, Generic)

newtype AgentRunId = AgentRunId {unAgentRunId :: Text}
  deriving stock (Eq, Ord, Show, Generic)

newtype AttemptId = AttemptId {unAttemptId :: Int}
  deriving stock (Eq, Ord, Show, Generic)

newtype ForkId = ForkId {unForkId :: Text}
  deriving stock (Eq, Ord, Show, Generic)

newtype ArtifactId = ArtifactId {unArtifactId :: Text}
  deriving stock (Eq, Ord, Show, Generic)

newtype CheckpointId = CheckpointId {unCheckpointId :: Text}
  deriving stock (Eq, Ord, Show, Generic)

data AgentIdentity = AgentIdentity
  { identityAgentId :: AgentId
  , identityRunId :: AgentRunId
  , identityParentRunId :: Maybe AgentRunId
  }
  deriving stock (Eq, Ord, Show, Generic)

data AgentNodePointer = AgentNodePointer
  { pointerNode :: GraphNodeRef
  , pointerAttempt :: AttemptId
  }
  deriving stock (Eq, Ord, Show, Generic)

data HistorySegment = HistorySegment
  { segmentNode :: AgentNodePointer
  , segmentHistory :: LlmHistory
  , segmentSummary :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

newtype HistoryTrace = HistoryTrace
  { unHistoryTrace :: [HistorySegment]
  }
  deriving stock (Eq, Show, Generic)

data HistorySelectionPolicy
  = SelectCurrentNode
  | SelectCurrentAndPredecessors
  | SelectReachableDependencies
  | SelectExplicitNodes [AgentNodePointer]
  deriving stock (Eq, Show, Generic)

data ForkTarget = ForkTarget
  { forkTargetNode :: GraphNodeRef
  , forkTargetAttempt :: AttemptId
  }
  deriving stock (Eq, Ord, Show, Generic)

data ForkRequest = ForkRequest
  { forkRequestId :: ForkId
  , forkRequestParent :: AgentRunId
  , forkRequestTarget :: ForkTarget
  , forkRequestSelectedHistory :: [HistorySegment]
  }
  deriving stock (Eq, Show, Generic)

data RunningSubagent = RunningSubagent
  { runningSubagentRequest :: ForkRequest
  , runningSubagentHandle :: Async (Either AgentError AgentRunResult)
  }
  deriving stock Generic

data NodeRunStatus
  = NodeNotStarted
  | NodeReady
  | NodeRunning AgentRunId
  | NodeBlocked Text
  | NodeCompleted
  | NodeFailed AgentError
  deriving stock (Eq, Show, Generic)

data EdgeDecision = EdgeDecision
  { edgeDecisionEdge :: SkillEdge
  , edgeDecisionSelected :: Bool
  , edgeDecisionReason :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data BudgetState = BudgetState
  { budgetMaxLlmCalls :: Maybe Int
  , budgetUsedLlmCalls :: Int
  , budgetMaxToolCalls :: Maybe Int
  , budgetUsedToolCalls :: Int
  , budgetMaxSubagents :: Maybe Int
  , budgetUsedSubagents :: Int
  }
  deriving stock (Eq, Show, Generic)

data AgentEnv = AgentEnv
  { envExecutionGraph :: SkillFlowGraph
  , envIdentity :: AgentIdentity
  , envCurrentNode :: AgentNodePointer
  , envProvidedHistory :: [HistorySegment]
  , envHistorySelectionPolicy :: HistorySelectionPolicy
  , envModel :: Maybe ModelName
  , envToolRegistry :: Map ToolName ToolSpec
  }
  deriving stock (Eq, Show, Generic)

data AgentState = AgentState
  { stateRunningSubagents :: Map AgentRunId RunningSubagent
  , stateHistoryTrace :: HistoryTrace
  , stateOwnHistory :: LlmHistory
  , stateActiveTools :: Map ToolName ToolSpec
  , stateActiveToolCalls :: Map ToolCallId ToolCall
  , stateParallelToolCalls :: Map ToolCallId ToolCall
  , stateCompletedToolOutputs :: Map ToolCallId ToolOutput
  , stateNodeStatuses :: Map GraphNodeRef NodeRunStatus
  , stateEdgeDecisions :: [EdgeDecision]
  , stateReadyNodes :: [GraphNodeRef]
  , stateScheduledNodes :: [GraphNodeRef]
  , stateProducedArtifacts :: [ArtifactId]
  , stateLastLlmRequest :: Maybe LlmRequest
  , stateLastLlmResponse :: Maybe LlmResponse
  , stateBudget :: BudgetState
  }
  deriving stock Generic

data AgentEvent
  = AgentStarted AgentIdentity
  | AgentFinished AgentRunId
  | AgentFailed AgentRunId AgentError
  | NodeStarted AgentNodePointer
  | NodeFinished AgentNodePointer
  | NodeSkipped AgentNodePointer Text
  | NodeFailedEvent AgentNodePointer AgentError
  | EdgeSelected EdgeDecision
  | HistorySelected ForkId [HistorySegment]
  | LlmRequestStarted AgentRunId LlmRequest
  | LlmResponseReceived AgentRunId LlmResponse
  | LlmCallFailed AgentRunId AgentError
  | ToolCallStarted ToolCall
  | ToolCallFinished ToolOutput
  | ToolCallFailed ToolCallId Text
  | ParallelToolGroupStarted [ToolCallId]
  | ParallelToolGroupFinished [ToolCallId]
  | SubagentForked ForkRequest
  | SubagentFinished AgentRunId AgentRunResult
  | SubagentFailed AgentRunId AgentError
  | SubagentJoined AgentRunId
  | SchedulerDecision Text
  | BudgetUpdated BudgetState
  | ArtifactProduced ArtifactId
  | CheckpointWritten CheckpointId
  deriving stock (Eq, Show, Generic)

data AgentError
  = AgentLlmError Text
  | AgentToolError Text
  | AgentGraphError Text
  | AgentBudgetExceeded Text
  | AgentSubagentFailed AgentRunId Text
  | AgentCancelled Text
  deriving stock (Eq, Ord, Show, Generic)

data AgentRunResult = AgentRunResult
  { runResultAgent :: AgentRunId
  , runResultNode :: AgentNodePointer
  , runResultHistory :: [HistorySegment]
  , runResultArtifacts :: [ArtifactId]
  }
  deriving stock (Eq, Show, Generic)

newtype Agentic a = Agentic
  { unAgentic
      :: RWST AgentEnv [AgentEvent] AgentState (ExceptT AgentError IO) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AgentEnv
    , MonadState AgentState
    , MonadWriter [AgentEvent]
    , MonadError AgentError
    )

runAgentic
  :: AgentEnv
  -> AgentState
  -> Agentic a
  -> IO (Either AgentError (a, AgentState, [AgentEvent]))
runAgentic env state action =
  runExceptT (runRWST (unAgentic action) env state)

evalAgentic :: AgentEnv -> AgentState -> Agentic a -> IO (Either AgentError a)
evalAgentic env state action =
  fmap (\(result, _, _) -> result) <$> runAgentic env state action

execAgentic
  :: AgentEnv -> AgentState -> Agentic a -> IO (Either AgentError AgentState)
execAgentic env state action =
  fmap (\(_, state', _) -> state') <$> runAgentic env state action

tellEvent :: AgentEvent -> Agentic ()
tellEvent event =
  tell [event]

instance Show RunningSubagent where
  show running =
    "RunningSubagent {runningSubagentRequest = "
      <> show (runningSubagentRequest running)
      <> ", runningSubagentHandle = <async>}"

instance Show AgentState where
  show state =
    "AgentState"
      <> " { stateRunningSubagents = "
      <> show (stateRunningSubagents state)
      <> ", stateHistoryTrace = "
      <> show (stateHistoryTrace state)
      <> ", stateOwnHistory = "
      <> show (stateOwnHistory state)
      <> ", stateActiveTools = "
      <> show (stateActiveTools state)
      <> ", stateActiveToolCalls = "
      <> show (stateActiveToolCalls state)
      <> ", stateParallelToolCalls = "
      <> show (stateParallelToolCalls state)
      <> ", stateCompletedToolOutputs = "
      <> show (stateCompletedToolOutputs state)
      <> ", stateNodeStatuses = "
      <> show (stateNodeStatuses state)
      <> ", stateEdgeDecisions = "
      <> show (stateEdgeDecisions state)
      <> ", stateReadyNodes = "
      <> show (stateReadyNodes state)
      <> ", stateScheduledNodes = "
      <> show (stateScheduledNodes state)
      <> ", stateProducedArtifacts = "
      <> show (stateProducedArtifacts state)
      <> ", stateLastLlmRequest = "
      <> show (stateLastLlmRequest state)
      <> ", stateLastLlmResponse = "
      <> show (stateLastLlmResponse state)
      <> ", stateBudget = "
      <> show (stateBudget state)
      <> " }"
