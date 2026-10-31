module Main (main) where

import Agent.Backends.GPT (gptRequestJson)
import Agent.Backends.Gemini (geminiRequestJson)
import Agent.PickyFork.Agentic
import Agent.PickyFork.Llm hiding (ToolCallFailed)
import Agent.SSL
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (forM_, unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS.Strict (ask)
import Data.Aeson
  ( Value (..)
  , eitherDecodeStrict'
  , encode
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IORef
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.Timeout (timeout)

main :: IO ()
main = do
  bytes <- BS.readFile "test/fixtures/ssl/analyze-project.json"
  record <- success (eitherDecodeStrict' bytes)
  let skill = buildSslSkill record
  let graph = sslSkillGraph skill
  selectionTest graph
  workflowTest skill
  forkLoopTest graph
  nestedForkTest graph
  modelDelegationTest graph
  limitsTest graph
  cleanupTest graph
  putStrLn "All runtime tests passed."

assert :: String -> Bool -> IO ()
assert label ok = unless ok (fail label)

success :: Show e => Either e a -> IO a
success = either (fail . show) pure

node :: Text -> AgentNodePointer
node name =
  AgentNodePointer
    (LogicStepRef (SkillId "analyze-project") (LogicStepId name))
    (AttemptId 1)

a, b, c, unrelated :: AgentNodePointer
a = node "step_read_summaries"
b = node "step_classify_scope_change"
c = node "step_assign_root_cause"
unrelated = node "unrelated"

target :: AgentNodePointer -> ForkTarget
target pointer = ForkTarget (pointerNode pointer) (pointerAttempt pointer)

environment :: SkillFlowGraph -> AgentEnv
environment graph =
  AgentEnv
    graph
    (AgentIdentity (AgentId "master") (AgentRunId "master-run") Nothing)
    a
    []
    SelectCurrentAndPredecessors
    Nothing
    Map.empty

input :: Text -> LlmInputItem
input text = InputMessage (message UserRole text)

message :: Role -> Text -> LlmMessage
message role text = LlmMessage Nothing role [TextPart text] Nothing Map.empty

segment :: AgentNodePointer -> Text -> HistorySegment
segment pointer text = HistorySegment pointer (LlmHistory [input text] []) Nothing

requestTemplate :: LlmRequest
requestTemplate =
  LlmRequest
    Nothing
    Nothing
    (ModelName "scripted")
    []
    []
    ToolAuto
    (GenerationParams Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing)
    Map.empty

config :: (LlmRequest -> IO (Either LlmError LlmResponse)) -> AgentLoopConfig
config model = AgentLoopConfig requestTemplate 8 model (\_ -> pure (Left "unknown tool"))

finalResponse :: Text -> LlmResponse
finalResponse text = LlmResponse [ResponseMessage (message AssistantRole text)]

edge :: AgentNodePointer -> AgentNodePointer -> SkillEdge
edge from to = SkillEdge (pointerNode from) (pointerNode to) LogicStepNext Nothing

selectionTest :: SkillFlowGraph -> IO ()
selectionTest graph = do
  let
    g = graph{flowEdges = [edge a b, edge b c, edge c a]}
    sa = segment a "A"
    sb = segment b "B"
    sc = segment c "C"
    state =
      initialAgentState
        { stateHistoryTrace = HistoryTrace [sa, sb, sc, segment unrelated "secret"]
        }
    env = (environment g){envProvidedHistory = [sa]}
  ((direct, transitive, explicit), _, _) <-
    runAgentic
      env
      state
      ( do
          d <- pickHistoryWith SelectCurrentAndPredecessors (target c)
          t <- pickHistoryWith SelectReachableDependencies (target c)
          e <- pickHistoryWith (SelectExplicitNodes [b]) (target c)
          pure (pickedSegments d, pickedSegments t, pickedSegments e)
      )
      >>= success
  assert "direct dependencies and target, no duplicates" (direct == [sb, sc])
  assert
    "transitive dependency cycle terminates and deduplicates"
    (transitive == [sa, sb, sc])
  assert "explicit selection uses exact attempt" (explicit == [sb])
  (wrongAttempt, _, _) <-
    runAgentic
      env
      state
      ( pickHistoryWith
          (SelectExplicitNodes [b{pointerAttempt = AttemptId 2}])
          (target c)
      )
      >>= success
  assert "other attempts excluded" (null (pickedSegments wrongAttempt))
  putStrLn "PASS selection, cycles, attempts and deduplication"

workflowTest :: SslSkill -> IO ()
workflowTest skill = do
  requests <- newIORef []
  let
    model request = do
      modifyIORef' requests (<> [request])
      pure (Right (finalResponse "done"))
    interpreter step = do
      env <- ask
      liftIO
        ( assert
            "workflow updates current node"
            ( pointerNode (envCurrentNode env)
                == LogicStepRef (SkillId "analyze-project") (logicStepId step)
            )
        )
      runAgentLoop (config model) [input (unLogicStepId (logicStepId step))]
    workflow = defaultWorkflowConfig{workflowStepInterpreter = interpreter}
    graph = sslSkillGraph skill
  (_, state, _) <-
    runAgentic (environment graph) initialAgentState (runWorkflow workflow skill)
      >>= success
  seen <- readIORef requests
  let segments = unHistoryTrace (stateHistoryTrace state)
  assert
    "workflow fixture executes all 30 steps"
    (length seen == 30 && length segments == 30)
  forM_ (zip seen segments) $ \(request, own) -> do
    let
      preds = directControlPredecessors graph (pointerNode (segmentNode own))
      prior = takeWhile ((/= segmentNode own) . segmentNode) segments
      inherited = filter ((`elem` preds) . pointerNode . segmentNode) prior
      ownInputs = historyInputItems (segmentHistory own)
    assert "workflow saves only own prompt and answer" (length ownInputs == 2)
    assert
      "workflow constructs model input from direct dependencies only"
      ( requestInput request
          == historyInputItems (selectedHistory inherited) <> take 1 ownInputs
      )
  putStrLn "PASS workflow dependency handoff through real model/tool loop"

forkLoopTest :: SkillFlowGraph -> IO ()
forkLoopTest graph = do
  seen <- newIORef []
  rounds <- newIORef (0 :: Int)
  let
    call =
      ToolCall (ToolCallId "lookup-1") (ToolName "lookup") Nothing (object []) Nothing
    output =
      ToolOutput
        (toolCallId call)
        (Just (toolCallName call))
        [ToolOutputText "found"]
        (Just True)
    model request = do
      modifyIORef' seen (<> [request])
      n <- atomicModifyIORef' rounds (\n -> (n + 1, n))
      pure
        ( Right
            ( if n == 0
                then LlmResponse [ResponseToolCall call]
                else finalResponse "child done"
            )
        )
    child = (config model){loopRunTool = \_ -> pure (Right output)}
    state =
      initialAgentState
        { stateHistoryTrace =
            HistoryTrace [segment a "relevant", segment unrelated "SECRET"]
        }
  (result, finalState, events) <-
    runAgentic
      (environment graph)
      state
      ( withSubagent
          (ForkId "fork-1")
          (AgentRunId "child-1")
          (SelectExplicitNodes [a])
          (target b)
          (runAgentLoop child [input "child task"])
          joinSubagent
      )
      >>= success
  requests <- readIORef seen
  ownSegment <- case runResultHistory result of
    [one] -> pure one
    _ -> fail "expected one child segment"
  (firstRequest, secondRequest) <- case requests of
    [first, second] -> pure (first, second)
    _ -> fail "expected two model rounds"
  let
    expected = [input "relevant", input "child task"]
    own = historyInputItems (segmentHistory ownSegment)
  assert
    "fork passes exact selected prefix"
    ( map requestInput requests
        == [ expected
           , expected <> [InputResponse (ResponseToolCall call), InputToolOutput output]
           ]
    )
  assert
    "child stores only own history"
    ( own
        == [ input "child task"
           , InputResponse (ResponseToolCall call)
           , InputToolOutput output
           , InputResponse (ResponseMessage (message AssistantRole "child done"))
           ]
    )
  assert
    "join collects child history and clears worker"
    ( length (unHistoryTrace (stateHistoryTrace finalState)) == 3
        && Map.null (stateRunningSubagents finalState)
    )
  assert
    "child events returned to parent"
    (SubagentJoined (AgentRunId "child-1") `elem` events && any isRequest events)
  let
    wire =
      BL.unpack
        ( encode
            (gptRequestJson firstRequest{requestInput = requestInput secondRequest})
        )
    geminiWire = BL.unpack (encode (geminiRequestJson secondRequest))
  assert
    "OpenAI replay includes top-level tool call before output"
    ( "\"type\":\"function_call\"" `isInfixOf` wire
        && "\"type\":\"function_call_output\"" `isInfixOf` wire
        && "\"arguments\":\"{}\"" `isInfixOf` wire
    )
  inputWire <-
    success
      (parseEither (withObject "request" (.: "input")) (gptRequestJson secondRequest))
  assert
    "OpenAI call and result are top-level, ordered and carry matching IDs"
    ( (inputWire :: [Value])
        == [ object
               [ "role" .= String "user"
               , "content"
                   .= [object ["type" .= String "input_text", "text" .= String "relevant"]]
               ]
           , object
               [ "role" .= String "user"
               , "content"
                   .= [object ["type" .= String "input_text", "text" .= String "child task"]]
               ]
           , object
               [ "type" .= String "function_call"
               , "call_id" .= String "lookup-1"
               , "name" .= String "lookup"
               , "arguments" .= String "{}"
               ]
           , object
               [ "type" .= String "function_call_output"
               , "call_id" .= String "lookup-1"
               , "output" .= String "found"
               ]
           ]
    )
  assert
    "Gemini replay includes function call and response"
    ( "functionCall" `isInfixOf` geminiWire
        && "functionResponse" `isInfixOf` geminiWire
    )
  let jsonOutput =
        ToolOutput
          (ToolCallId "catalog")
          Nothing
          [ToolOutputJson (object ["key" .= String "node-1"])]
          (Just True)
  jsonWire <-
    success
      ( parseEither
          (withObject "request" (.: "input"))
          (gptRequestJson requestTemplate{requestInput = [InputToolOutput jsonOutput]})
      )
  assert
    "structured tool output is valid JSON text, not Haskell Show"
    ( (jsonWire :: [Value])
        == [ object
               [ "type" .= String "function_call_output"
               , "call_id" .= String "catalog"
               , "output" .= String "{\"key\":\"node-1\"}"
               ]
           ]
    )
  putStrLn
    "PASS fork, model/tool replay, isolated delta, join and provider serialization"
 where
  isRequest LlmRequestStarted{} = True
  isRequest _ = False

nestedForkTest :: SkillFlowGraph -> IO ()
nestedForkTest graph = do
  firstStarted <- newEmptyMVar
  secondStarted <- newEmptyMVar
  let
    firstTask =
      liftIO (putMVar firstStarted () >> takeMVar secondStarted)
        >> pure (LlmHistory [input "first child"] [])
    secondTask =
      liftIO (takeMVar firstStarted >> putMVar secondStarted ())
        >> pure (LlmHistory [input "second child"] [])
    run = withSubagent
      (ForkId "first")
      (AgentRunId "first")
      (SelectExplicitNodes [])
      (target a)
      firstTask
      $ \_ ->
        withSubagent
          (ForkId "second")
          (AgentRunId "second")
          (SelectExplicitNodes [])
          (target b)
          secondTask
          (\_ -> pure ())
  outcome <-
    timeout 2000000 (runAgentic (environment graph) initialAgentState run)
  (_, state, _) <-
    maybe (fail "scoped child execution should overlap") success outcome
  assert
    "automatic join collects both overlapping workers"
    ( Map.null (stateRunningSubagents state)
        && length (unHistoryTrace (stateHistoryTrace state)) == 2
    )
  let
    grandchild = pure (LlmHistory [input "grandchild"] [])
    child = do
      _ <-
        withSubagent
          (ForkId "grandchild")
          (AgentRunId "grandchild")
          (SelectExplicitNodes [])
          (target c)
          grandchild
          joinSubagent
      pure (LlmHistory [input "child"] [])
  (result, _, _) <-
    runAgentic
      (environment graph)
      initialAgentState
      ( withSubagent
          (ForkId "child")
          (AgentRunId "child")
          (SelectExplicitNodes [])
          (target b)
          child
          joinSubagent
      )
      >>= success
  assert
    "nested fork descendants are returned on join"
    (map segmentNode (runResultHistory result) == [c, b])
  putStrLn "PASS concurrent workers, automatic joins and nested result collection"

modelDelegationTest :: SkillFlowGraph -> IO ()
modelDelegationTest graph = do
  seen <- newIORef []
  rounds <- newIORef (0 :: Int)
  let
    childModel request =
      modifyIORef' seen (<> [request]) >> pure (Right (finalResponse "slave result"))
    child = config childModel
    targets = [DelegationTarget "analysis" "Analyze the relevant evidence" (target b)]
    mkCall name args =
      ResponseToolCall
        (ToolCall (ToolCallId name) (ToolName name) Nothing args Nothing)
    masterModel _ = do
      n <- atomicModifyIORef' rounds (\n -> (n + 1, n))
      pure
        ( Right
            ( case n of
                0 -> LlmResponse [mkCall "list_context" (object [])]
                1 ->
                  LlmResponse
                    [ mkCall
                        "delegate_task"
                        ( object
                            [ "target" .= String "analysis"
                            , "task" .= String "solve"
                            , "history" .= [historyKey a]
                            ]
                        )
                    ]
                _ -> finalResponse "master done"
            )
        )
    master =
      (config masterModel)
        { loopRequestTemplate = requestTemplate{requestTools = delegationTools targets}
        , loopRunTool = runDelegationTool child targets
        }
    state =
      initialAgentState
        { stateHistoryTrace =
            HistoryTrace [segment a "selected evidence", segment unrelated "SECRET"]
        }
  (_, finalState, events) <-
    runAgentic
      (environment graph)
      state
      (executeNodeTask c (runAgentLoop master [input "delegate relevant analysis"]))
      >>= success
  requests <- readIORef seen
  assert
    "master chooses slave context by model-generated keys"
    (map requestInput requests == [[input "selected evidence", input "solve"]])
  assert
    "master and slave histories both retained"
    (length (unHistoryTrace (stateHistoryTrace finalState)) == 4)
  let forks = [request | SubagentForked request <- events]
  assert
    "fork records exact model selection"
    (map (map segmentNode . forkRequestSelectedHistory) forks == [[a]])
  -- Forking while a parent is running must expose its new messages but must
  -- not forward the pending fork call or re-attribute its inherited context.
  let
    pending =
      ToolCall
        (ToolCallId "pending")
        (ToolName "delegate_task")
        Nothing
        (object [])
        Nothing
    env =
      (environment graph)
        { envCurrentNode = c
        , envProvidedHistory = [segment a "inherited"]
        }
    liveState =
      initialAgentState
        { stateNodeStatuses =
            Map.singleton (pointerNode c) (NodeRunning (AgentRunId "master-run"))
        , stateOwnHistory =
            LlmHistory
              [input "inherited", input "own", InputResponse (ResponseToolCall pending)]
              []
        }
  (available, _, _) <- runAgentic env liveState availableHistory >>= success
  assert
    "running parent exposes only its own completed context"
    ( map segmentHistory available
        == [segmentHistory (segment a "inherited"), LlmHistory [input "own"] []]
    )
  putStrLn "PASS model-driven selective delegation and running parent context"

limitsTest :: SkillFlowGraph -> IO ()
limitsTest graph = do
  modelCalls <- newIORef (0 :: Int)
  toolCalls <- newIORef (0 :: Int)
  let
    model _ = modifyIORef' modelCalls (+ 1) >> pure (Right (finalResponse "done"))
    zeroBudget = (stateBudget initialAgentState){budgetMaxLlmCalls = Just 0}
  result <-
    runAgentic
      (environment graph)
      initialAgentState{stateBudget = zeroBudget}
      (executeNodeTask a (runAgentLoop (config model) []))
  assert
    "zero LLM budget fails"
    (case result of Left AgentBudgetExceeded{} -> True; _ -> False)
  count <- readIORef modelCalls
  assert "budget checked before model call" (count == 0)
  let
    call = ToolCall (ToolCallId "tool") (ToolName "tool") Nothing (object []) Nothing
    toolConfig =
      (config (\_ -> pure (Right (LlmResponse [ResponseToolCall call]))))
        { loopRunTool = \_ -> liftIO (modifyIORef' toolCalls (+ 1)) >> pure (Left "failed")
        }
    noTools = (stateBudget initialAgentState){budgetMaxToolCalls = Just 0}
  result2 <-
    runAgentic
      (environment graph)
      initialAgentState{stateBudget = noTools}
      (executeNodeTask a (runAgentLoop toolConfig []))
  assert
    "tool budget fails before effects"
    (case result2 of Left AgentBudgetExceeded{} -> True; _ -> False)
  effects <- readIORef toolCalls
  assert "no tool effect on exhausted budget" (effects == 0)
  result3 <-
    runAgentic
      (environment graph)
      initialAgentState
      ( withSubagent
          (ForkId "bad")
          (AgentRunId "bad-child")
          (SelectExplicitNodes [])
          (target b)
          (throwError (AgentToolError "child failure"))
          joinSubagent
      )
  assert
    "child error propagates"
    (case result3 of Left (AgentToolError "child failure") -> True; _ -> False)
  result4 <-
    runAgentic
      (environment graph)
      initialAgentState
      (executeNodeTask a (runAgentLoop (config model){loopMaxTurns = 0} []))
  assert
    "turn budget bounded"
    (case result4 of Left AgentBudgetExceeded{} -> True; _ -> False)
  result5 <-
    runAgentic
      (environment graph)
      initialAgentState
      ( runDelegationTool
          (config model)
          [DelegationTarget "x" "x" (target b)]
          ( ToolCall
              (ToolCallId "invalid")
              (ToolName "delegate_task")
              Nothing
              ( object
                  ["target" .= String "x", "task" .= String "x", "history" .= [String "unknown"]]
              )
              Nothing
          )
      )
  case result5 of
    Right (Left _, _, _) -> pure ()
    _ -> fail "invalid model history key must not silently start a child"
  putStrLn "PASS budgets, child failure and invalid model selections"

cleanupTest :: SkillFlowGraph -> IO ()
cleanupTest graph = do
  started <- newEmptyMVar
  blocker <- newEmptyMVar
  stopped <- newEmptyMVar
  let
    task =
      liftIO
        ( (putMVar started () >> takeMVar blocker >> pure emptyHistory)
            `finally` putMVar stopped ()
        )
    parent _ = liftIO (takeMVar started) >> throwError (AgentCancelled "parent stopped")
  outcome <-
    timeout 2000000 $
      runAgentic
        (environment graph)
        initialAgentState
        ( withSubagent
            (ForkId "cleanup")
            (AgentRunId "cleanup-child")
            (SelectExplicitNodes [])
            (target b)
            task
            parent
            :: Agentic ()
        )
  assert
    "parent error cancels scoped worker promptly"
    (case outcome of Just (Left (AgentCancelled _)) -> True; _ -> False)
  cleaned <- timeout 2000000 (takeMVar stopped)
  assert "worker finalizer ran before scope exits" (cleaned == Just ())
  putStrLn "PASS worker cancellation on parent failure"
