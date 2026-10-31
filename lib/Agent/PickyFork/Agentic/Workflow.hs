module Agent.PickyFork.Agentic.Workflow
  ( WorkflowConfig (..)
  , WorkflowRunResult (..)
  , defaultWorkflowConfig
  , defaultWorkflowEdgeSelector
  , runWorkflow
  )
where

import Agent.PickyFork.Agentic.Runner (executeNodeTask)
import Agent.PickyFork.Agentic.Types
import Agent.PickyFork.Llm (LlmHistory, emptyHistory)
import Agent.SSL
  ( EdgeKind (LogicStepNext, SceneNext)
  , GraphNodeRef (LogicStepRef, SceneRef, TerminalRef)
  , LogicStepId
  , LogicStepNode
  , SceneId
  , SkillEdge (..)
  , SkillFlowGraph (..)
  , SkillId
  , SslSkill (..)
  , flowEdges
  , flowLogicSteps
  , flowScenes
  , flowSkill
  , logicStepId
  , sceneContainedLogicSteps
  , sceneEntryLogicStepId
  , sceneId
  , skillEntrySceneId
  , skillId
  )
import Control.Monad.Except (throwError)
import Control.Monad.RWS.Strict (get, local)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

data WorkflowConfig = WorkflowConfig
  { workflowMaxTransitions :: Int
  , workflowStepInterpreter :: LogicStepNode -> Agentic LlmHistory
  , workflowEdgeSelector :: GraphNodeRef -> [SkillEdge] -> Agentic (Maybe SkillEdge)
  }

data WorkflowRunResult = WorkflowRunResult
  { workflowRunSkill :: SkillId
  , workflowRunTerminal :: Maybe GraphNodeRef
  , workflowRunSteps :: [AgentRunResult]
  }
  deriving stock (Eq, Show, Generic)

defaultWorkflowConfig :: WorkflowConfig
defaultWorkflowConfig =
  WorkflowConfig
    { workflowMaxTransitions = 512
    , workflowStepInterpreter = const (pure emptyHistory)
    , workflowEdgeSelector = defaultWorkflowEdgeSelector
    }

defaultWorkflowEdgeSelector :: GraphNodeRef -> [SkillEdge] -> Agentic (Maybe SkillEdge)
defaultWorkflowEdgeSelector _ [] = pure Nothing
defaultWorkflowEdgeSelector _ (edge : _) = pure (Just edge)

runWorkflow :: WorkflowConfig -> SslSkill -> Agentic WorkflowRunResult
runWorkflow config skill = do
  let graph = sslSkillGraph skill
      sid = skillId (flowSkill graph)
  entryScene <-
    maybe
      (throwWorkflowError "SSL skill has no entry scene")
      pure
      (skillEntrySceneId (flowSkill graph))
  local (\env -> env{envExecutionGraph = graph}) $
    runFrom config graph sid [] (workflowMaxTransitions config) (SceneRef sid entryScene)

runFrom
  :: WorkflowConfig
  -> SkillFlowGraph
  -> SkillId
  -> [AgentRunResult]
  -> Int
  -> GraphNodeRef
  -> Agentic WorkflowRunResult
runFrom _ _ sid results _ terminal@(TerminalRef _ _) =
  pure
    WorkflowRunResult
      { workflowRunSkill = sid
      , workflowRunTerminal = Just terminal
      , workflowRunSteps = reverse results
      }
runFrom config graph sid results remaining node
  | remaining <= 0 =
      throwWorkflowError "workflow transition budget exceeded"
  | otherwise =
      case node of
        SceneRef _ sceneId' ->
          runScene config graph sid results remaining sceneId'
        LogicStepRef _ stepId ->
          runLogicStep config graph sid results remaining stepId
        _ ->
          throwWorkflowError ("unsupported workflow node: " <> showText node)

runScene
  :: WorkflowConfig
  -> SkillFlowGraph
  -> SkillId
  -> [AgentRunResult]
  -> Int
  -> SceneId
  -> Agentic WorkflowRunResult
runScene config graph sid results remaining sceneId' = do
  scene <-
    maybe
      (throwWorkflowError ("missing scene: " <> showText sceneId'))
      pure
      (Map.lookup sceneId' (flowScenes graph))
  case sceneEntryLogicStepId scene of
    Just entryStep ->
      runFrom
        config
        graph
        sid
        results
        (remaining - 1)
        (LogicStepRef sid entryStep)
    Nothing ->
      selectAndFollow
        config
        graph
        sid
        results
        (remaining - 1)
        (SceneRef sid sceneId')
        SceneNext

runLogicStep
  :: WorkflowConfig
  -> SkillFlowGraph
  -> SkillId
  -> [AgentRunResult]
  -> Int
  -> LogicStepId
  -> Agentic WorkflowRunResult
runLogicStep config graph sid results remaining stepId = do
  step <-
    maybe
      (throwWorkflowError ("missing logic step: " <> showText stepId))
      pure
      (Map.lookup stepId (flowLogicSteps graph))
  attempt <- nextAttemptFor (LogicStepRef sid stepId)
  result <-
    executeNodeTask
      AgentNodePointer
        { pointerNode = LogicStepRef sid stepId
        , pointerAttempt = attempt
        }
      (workflowStepInterpreter config step)
  followLogicStepExit config graph sid (result : results) (remaining - 1) step

followLogicStepExit
  :: WorkflowConfig
  -> SkillFlowGraph
  -> SkillId
  -> [AgentRunResult]
  -> Int
  -> LogicStepNode
  -> Agentic WorkflowRunResult
followLogicStepExit config graph sid results remaining step =
  selectEdge config (LogicStepRef sid (logicStepId step)) LogicStepNext graph >>= \case
    Nothing ->
      pure
        WorkflowRunResult
          { workflowRunSkill = sid
          , workflowRunTerminal = Nothing
          , workflowRunSteps = reverse results
          }
    Just edge ->
      case edgeTo edge of
        TerminalRef _ terminal
          | terminal == "YIELD_SUCCESS" || terminal == "YIELD_FAIL" ->
              followContainingScene config graph sid results remaining (logicStepId step)
        next ->
          runFrom config graph sid results remaining next

followContainingScene
  :: WorkflowConfig
  -> SkillFlowGraph
  -> SkillId
  -> [AgentRunResult]
  -> Int
  -> LogicStepId
  -> Agentic WorkflowRunResult
followContainingScene config graph sid results remaining stepId =
  case containingScene graph stepId of
    Nothing ->
      pure
        WorkflowRunResult
          { workflowRunSkill = sid
          , workflowRunTerminal = Just (TerminalRef sid "YIELD_SUCCESS")
          , workflowRunSteps = reverse results
          }
    Just sceneId' ->
      selectAndFollow
        config
        graph
        sid
        results
        remaining
        (SceneRef sid sceneId')
        SceneNext

selectAndFollow
  :: WorkflowConfig
  -> SkillFlowGraph
  -> SkillId
  -> [AgentRunResult]
  -> Int
  -> GraphNodeRef
  -> EdgeKind
  -> Agentic WorkflowRunResult
selectAndFollow config graph sid results remaining node kind =
  selectEdge config node kind graph >>= \case
    Nothing ->
      pure
        WorkflowRunResult
          { workflowRunSkill = sid
          , workflowRunTerminal = Nothing
          , workflowRunSteps = reverse results
          }
    Just edge ->
      runFrom config graph sid results remaining (edgeTo edge)

selectEdge
  :: WorkflowConfig
  -> GraphNodeRef
  -> EdgeKind
  -> SkillFlowGraph
  -> Agentic (Maybe SkillEdge)
selectEdge config node kind graph = do
  let candidates =
        [ edge
        | edge <- flowEdges graph
        , edgeFrom edge == node
        , edgeKind edge == kind
        ]
  selected <- workflowEdgeSelector config node candidates
  traverse_ tellSelected selected
  pure selected

tellSelected :: SkillEdge -> Agentic ()
tellSelected edge =
  tellEvent $
    EdgeSelected
      EdgeDecision
        { edgeDecisionEdge = edge
        , edgeDecisionSelected = True
        , edgeDecisionReason = edgeCondition edge
        }

nextAttemptFor :: GraphNodeRef -> Agentic AttemptId
nextAttemptFor node = do
  state <- get
  let previousAttempts =
        [ pointerAttempt (segmentNode segment)
        | segment <- unHistoryTrace (stateHistoryTrace state)
        , pointerNode (segmentNode segment) == node
        ]
  pure (AttemptId (length previousAttempts + 1))

containingScene :: SkillFlowGraph -> LogicStepId -> Maybe SceneId
containingScene graph stepId =
  sceneId
    <$> find
      ( \scene ->
          sceneEntryLogicStepId scene == Just stepId
            || stepId `elem` sceneContainedLogicSteps scene
      )
      (Map.elems (flowScenes graph))

throwWorkflowError :: Text -> Agentic a
throwWorkflowError = throwError . AgentGraphError

showText :: Show a => a -> Text
showText = Text.pack . show

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find predicate (x : xs)
  | predicate x = Just x
  | otherwise = find predicate xs
