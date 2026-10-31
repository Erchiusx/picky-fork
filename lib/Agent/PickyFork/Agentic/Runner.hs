module Agent.PickyFork.Agentic.Runner
  ( PickedHistory (..)
  , pickHistory
  , pickHistoryWith
  , buildForkRequest
  , buildForkRequestWith
  , selectedHistory
  , availableHistory
  , executeNodeTask
  , appendHistorySegment
  , segmentsForNode
  , segmentsForNodes
  , directControlPredecessors
  , reachableControlPredecessors
  )
where

import Agent.PickyFork.Agentic.Types
import Agent.PickyFork.Llm
  ( LlmHistory (..)
  , LlmInputItem (..)
  , LlmResponseItem (..)
  , ToolCall (..)
  , ToolOutput (..)
  , combineHistories
  , historyInputItems
  )
import Agent.SSL
  ( EdgeKind (LogicStepNext, SceneNext)
  , GraphNodeRef
  , SkillEdge (..)
  , SkillFlowGraph
  , flowEdges
  )
import Control.Monad.Except (catchError, throwError)
import Control.Monad.RWS.Strict (ask, get, local, modify)
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)

data PickedHistory = PickedHistory
  { pickedTarget :: ForkTarget
  , pickedPolicy :: HistorySelectionPolicy
  , pickedSegments :: [HistorySegment]
  }
  deriving stock (Eq, Show, Generic)

pickHistory :: ForkTarget -> Agentic PickedHistory
pickHistory target = do
  env <- ask
  pickHistoryWith (envHistorySelectionPolicy env) target

pickHistoryWith :: HistorySelectionPolicy -> ForkTarget -> Agentic PickedHistory
pickHistoryWith policy target = do
  env <- ask
  allSegments <- availableHistory
  let
    node = forkTargetNode target
    selectedSegments =
      case policy of
        SelectCurrentNode ->
          segmentsForNode (pointerNode (envCurrentNode env)) allSegments
        SelectCurrentAndPredecessors ->
          segmentsForNodes
            (node : directControlPredecessors (envExecutionGraph env) node)
            allSegments
        SelectReachableDependencies ->
          segmentsForNodes
            (node : reachableControlPredecessors (envExecutionGraph env) node)
            allSegments
        SelectExplicitNodes pointers ->
          [ segment
          | segment <- allSegments
          , segmentNode segment `elem` pointers
          ]
  pure
    PickedHistory
      { pickedTarget = target
      , pickedPolicy = policy
      , pickedSegments = selectedSegments
      }

-- | Available node-owned histories, including the running parent's delta.
availableHistory :: Agentic [HistorySegment]
availableHistory = do
  env <- ask
  state <- get
  let
    -- A fork may happen inside a running parent's tool handler, before its
    -- segment is committed. Expose only its own delta, not its inherited prefix.
    currentOwnItems =
      drop
        (length (historyInputItems (selectedHistory (envProvidedHistory env))))
        (historyInputItems (stateOwnHistory state))
    runningSegment =
      [ HistorySegment
          (envCurrentNode env)
          (LlmHistory (completedItems currentOwnItems) [])
          Nothing
      | Map.lookup (pointerNode (envCurrentNode env)) (stateNodeStatuses state)
          == Just (NodeRunning (identityRunId (envIdentity env)))
      , not (null currentOwnItems)
      ]
    allSegments =
      reverse
        ( nubBy
            (\a b -> segmentNode a == segmentNode b)
            ( reverse
                ( envProvidedHistory env
                    <> traceSegments (stateHistoryTrace state)
                    <> runningSegment
                )
            )
        )
  pure allSegments

buildForkRequest :: ForkId -> ForkTarget -> Agentic ForkRequest
buildForkRequest forkId target = do
  env <- ask
  buildForkRequestWith (envHistorySelectionPolicy env) forkId target

buildForkRequestWith
  :: HistorySelectionPolicy -> ForkId -> ForkTarget -> Agentic ForkRequest
buildForkRequestWith policy forkId target = do
  env <- ask
  picked <- pickHistoryWith policy target
  let request =
        ForkRequest
          { forkRequestId = forkId
          , forkRequestParent = identityRunId (envIdentity env)
          , forkRequestTarget = target
          , forkRequestSelectedHistory = pickedSegments picked
          }
  tellEvent (HistorySelected forkId (pickedSegments picked))
  pure request

selectedHistory :: [HistorySegment] -> LlmHistory
selectedHistory = combineHistories . map segmentHistory

executeNodeTask
  :: AgentNodePointer
  -> Agentic LlmHistory
  -> Agentic AgentRunResult
executeNodeTask node task = do
  env <- ask
  picked <-
    local
      (\e -> e{envCurrentNode = node})
      (pickHistory (ForkTarget (pointerNode node) (pointerAttempt node)))
  let
    runId = identityRunId (envIdentity env)
    nodeRef = pointerNode node
  tellEvent (NodeStarted node)
  modify
    ( \state ->
        state
          { stateNodeStatuses =
              Map.insert nodeRef (NodeRunning runId) (stateNodeStatuses state)
          , stateOwnHistory = selectedHistory (pickedSegments picked)
          }
    )
  ( do
      history <-
        local
          (\e -> e{envCurrentNode = node, envProvidedHistory = pickedSegments picked})
          task
      let segment =
            HistorySegment
              { segmentNode = node
              , segmentHistory = history
              , segmentSummary = Nothing
              }
      appendHistorySegment segment
      modify
        ( \state ->
            state
              { stateOwnHistory = history
              , stateNodeStatuses =
                  Map.insert nodeRef NodeCompleted (stateNodeStatuses state)
              }
        )
      tellEvent (NodeFinished node)
      pure
        AgentRunResult
          { runResultAgent = runId
          , runResultNode = node
          , runResultHistory = [segment]
          , runResultArtifacts = []
          }
    )
    `catchError` \err -> do
      modify
        ( \state ->
            state
              { stateNodeStatuses =
                  Map.insert nodeRef (NodeFailed err) (stateNodeStatuses state)
              }
        )
      tellEvent (NodeFailedEvent node err)
      throwError err

appendHistorySegment :: HistorySegment -> Agentic ()
appendHistorySegment segment =
  modify
    ( \state ->
        state
          { stateHistoryTrace =
              HistoryTrace (traceSegments (stateHistoryTrace state) <> [segment])
          }
    )

segmentsForNode :: GraphNodeRef -> [HistorySegment] -> [HistorySegment]
segmentsForNode node =
  filter ((== node) . pointerNode . segmentNode)

segmentsForNodes :: [GraphNodeRef] -> [HistorySegment] -> [HistorySegment]
segmentsForNodes nodes segments =
  [ segment
  | segment <- segments
  , Set.member (pointerNode (segmentNode segment)) nodeSet
  ]
 where
  nodeSet = Set.fromList nodes

directControlPredecessors :: SkillFlowGraph -> GraphNodeRef -> [GraphNodeRef]
directControlPredecessors graph node =
  unique
    [ edgeFrom edge
    | edge <- flowEdges graph
    , edgeTo edge == node
    , isControlEdge (edgeKind edge)
    ]

reachableControlPredecessors :: SkillFlowGraph -> GraphNodeRef -> [GraphNodeRef]
reachableControlPredecessors graph node =
  go Set.empty [node]
 where
  go _ [] = []
  go seen (current : rest)
    | Set.member current seen = go seen rest
    | otherwise =
        let
          preds = directControlPredecessors graph current
          unseenPreds = filter (`Set.notMember` seen) preds
         in
          unseenPreds <> go (Set.insert current seen) (unseenPreds <> rest)

isControlEdge :: EdgeKind -> Bool
isControlEdge SceneNext = True
isControlEdge LogicStepNext = True
isControlEdge _ = False

traceSegments :: HistoryTrace -> [HistorySegment]
traceSegments = unHistoryTrace

unique :: Ord a => [a] -> [a]
unique = go Set.empty
 where
  go _ [] = []
  go seen (x : xs)
    | Set.member x seen = go seen xs
    | otherwise = x : go (Set.insert x seen) xs

-- Pending calls (notably the fork itself) cannot be replayed without outputs.
completedItems :: [LlmInputItem] -> [LlmInputItem]
completedItems items = filter complete items
 where
  outputIds =
    [toolOutputCallId output | InputToolOutput output <- items]
      <> [toolOutputCallId output | InputResponse (ResponseToolOutput output) <- items]
  complete (InputResponse (ResponseToolCall call)) = toolCallId call `elem` outputIds
  complete _ = True
