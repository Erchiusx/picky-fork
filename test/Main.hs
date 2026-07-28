module Main (main) where

import Agent.Backends.GPT
import Agent.Backends.Gemini
import Agent.PickyFork.Agentic
import Agent.PickyFork.Llm
import Agent.PickyFork.Llm.Transport.Http
import Agent.SSL
import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, hPutStrLn, openFile)

data TestLog = TestLog
  { testLogPath :: FilePath
  , testLogHandle :: Handle
  }

data AnalyzeProjectTestConfig = AnalyzeProjectTestConfig
  { analyzeProjectRoot :: FilePath
  , analyzeProjectBrainDir :: FilePath
  , analyzeProjectOutputDir :: FilePath
  }
  deriving stock (Eq, Show)

main :: IO ()
main = do
  logFile <- openTestLog
  putStrLn ("LLM integration log: " <> testLogPath logFile)
  sslOk <- runSslGraphFixtureTest logFile
  transport <- newHttpTransport
  openaiKey <- lookupEnv "OPENAI_API_KEY"
  geminiKey <- lookupEnv "GEMINI_API_KEY"
  openaiModel <- envOr "OPENAI_MODEL" "gpt-5.6-luna"
  geminiModel <- envOr "GEMINI_MODEL" "gemini-2.5-flash"

  openaiOk <-
    runProviderTest
      logFile
      "openai"
      openaiKey
      openaiModel
      (GPT (Text.pack (maybe "" id openaiKey)) defaultGPTBaseUrl transport)
      gptRequestJson

  geminiOk <-
    runProviderTest
      logFile
      "gemini"
      geminiKey
      geminiModel
      (Gemini (Text.pack (maybe "" id geminiKey)) defaultGeminiBaseUrl transport)
      geminiRequestJson

  hFlush (testLogHandle logFile)
  hClose (testLogHandle logFile)
  if sslOk && openaiOk && geminiOk
    then pure ()
    else exitFailure

runSslGraphFixtureTest :: TestLog -> IO Bool
runSslGraphFixtureTest logFile = do
  let fixturePath =
        "test"
          </> "fixtures"
          </> "ssl"
          </> "analyze-project.json"
      fixtureSlug = "analyze-project"
  decoded <- eitherDecodeStrict' <$> BS.readFile fixturePath
  case decoded of
    Left err -> do
      logSection logFile "ssl" ("fixture decode failed: " <> Text.pack err)
      putStatus "ssl" "FAIL"
      pure False
    Right record ->
      if recordSlug record /= fixtureSlug
        then do
          logSection logFile "ssl" ("wrong fixture slug: " <> recordSlug record)
          putStatus "ssl" "FAIL"
          pure False
        else runSslGraphFixture logFile fixtureSlug (buildSslSkill record)

runSslGraphFixture :: TestLog -> Text -> SslSkill -> IO Bool
runSslGraphFixture logFile fixtureSlug skill = do
  let graph = sslSkillGraph skill
  runnerOk <- runRunnerFixture logFile graph
  workflowOk <- runWorkflowFixture logFile skill
  let skillNode = flowSkill graph
      edgeCounts = skillEdgeCountsByKind skill
      checks =
        [ skillId skillNode == SkillId "analyze-project"
        , skillEntrySceneId skillNode == Just (SceneId "scene_discovery")
        , Map.member (SceneId "scene_discovery") (flowScenes graph)
        , Map.member (SceneId "scene_extraction") (flowScenes graph)
        , Map.member (SceneId "scene_analysis") (flowScenes graph)
        , Map.member (SceneId "scene_reporting") (flowScenes graph)
        , Map.member (SceneId "scene_optimization") (flowScenes graph)
        , Map.member (LogicStepId "step_read_summaries") (flowLogicSteps graph)
        , Map.member (LogicStepId "step_read_core_artifacts") (flowLogicSteps graph)
        , Map.member (LogicStepId "step_assign_root_cause") (flowLogicSteps graph)
        , Map.member (LogicStepId "step_populate_recommendations") (flowLogicSteps graph)
        , Map.member (LogicStepId "step_suggest_missing_skills") (flowLogicSteps graph)
        , skillSceneCount skill == 5
        , skillLogicStepCount skill == 30
        , Map.findWithDefault 0 SkillEntryScene edgeCounts == 1
        , Map.findWithDefault 0 SkillSubscene edgeCounts == 5
        , Map.findWithDefault 0 SceneContainsLogicStep edgeCounts == 30
        , Map.findWithDefault 0 SceneNext edgeCounts > 0
        , Map.findWithDefault 0 LogicStepNext edgeCounts > 0
        , runnerOk
        , workflowOk
        ]
      ok = and checks
  logSection
    logFile
    "ssl"
    ( "fixture: "
        <> fixtureSlug
        <> ", scenes="
        <> Text.pack (show (skillSceneCount skill))
        <> ", logic_steps="
        <> Text.pack (show (skillLogicStepCount skill))
        <> ", edges="
        <> Text.pack (show (skillEdgeCount skill))
        <> ", edge_counts_by_kind="
        <> Text.pack (show edgeCounts)
    )
  putStatus "ssl" (if ok then "PASS" else "FAIL")
  pure ok

runWorkflowFixture :: TestLog -> SslSkill -> IO Bool
runWorkflowFixture logFile skill = do
  analyzeConfig <- loadAnalyzeProjectTestConfig logFile
  let sid = SkillId "analyze-project"
      entryNode =
        AgentNodePointer
          { pointerNode = SceneRef sid (SceneId "scene_discovery")
          , pointerAttempt = AttemptId 1
          }
      env =
        AgentEnv
          { envExecutionGraph = sslSkillGraph skill
          , envIdentity =
              AgentIdentity
                { identityAgentId = AgentId "workflow-fixture-agent"
                , identityRunId = AgentRunId "workflow-fixture-run"
                , identityParentRunId = Nothing
                }
          , envCurrentNode = entryNode
          , envProvidedHistory = []
          , envHistorySelectionPolicy = SelectReachableDependencies
          , envModel = Nothing
          , envToolRegistry = Map.empty
          }
      state =
        emptyAgentState
          { stateHistoryTrace = HistoryTrace []
          , stateOwnHistory = emptyHistory
          }
      config =
        defaultWorkflowConfig
          { workflowMaxTransitions = 128
          , workflowStepInterpreter = interpretAnalyzeProjectStep analyzeConfig
          }
  result <- runAgentic env state (runWorkflow config skill)
  case result of
    Left err -> do
      logSection logFile "workflow" ("failed: " <> Text.pack (show err))
      pure False
    Right (workflowResult, finalState, _) -> do
      let stepCount = length (workflowRunSteps workflowResult)
          traceCount = length (unHistoryTrace (stateHistoryTrace finalState))
          projectMentioned =
            any
              (historyMentionsProject (analyzeProjectRoot analyzeConfig))
              (unHistoryTrace (stateHistoryTrace finalState))
          ok =
            and
              [ stepCount == 30
              , traceCount == 30
              , workflowRunTerminal workflowResult == Just (TerminalRef sid "END_SUCCESS")
              , projectMentioned
              ]
      logSection
        logFile
        "workflow"
        ( "steps="
            <> Text.pack (show stepCount)
            <> ", trace_segments="
            <> Text.pack (show traceCount)
            <> ", terminal="
            <> Text.pack (show (workflowRunTerminal workflowResult))
            <> ", project_root="
            <> Text.pack (analyzeProjectRoot analyzeConfig)
        )
      pure ok

loadAnalyzeProjectTestConfig :: TestLog -> IO AnalyzeProjectTestConfig
loadAnalyzeProjectTestConfig logFile = do
  maybeProject <- lookupEnv "PICKY_FORK_WORKFLOW_PROJECT"
  projectRoot <-
    case maybeProject of
      Just path -> pure path
      Nothing -> createAnalyzeProjectFixtureProject
  let config =
        AnalyzeProjectTestConfig
          { analyzeProjectRoot = projectRoot
          , analyzeProjectBrainDir = projectRoot </> "brain"
          , analyzeProjectOutputDir = "test-output" </> "workflow"
          }
  logSection logFile "workflow-config" (Text.pack (show config))
  pure config

createAnalyzeProjectFixtureProject :: IO FilePath
createAnalyzeProjectFixtureProject = do
  let projectRoot = "test-output" </> "fixtures" </> "cyclemod-like"
      brainDir = projectRoot </> "brain" </> "session-001"
  createDirectoryIfMissing True brainDir
  Text.writeFile
    (brainDir </> "task.md")
    "Analyze a local project fixture."
  Text.writeFile
    (brainDir </> "implementation_plan.md")
    "Read local artifacts and produce a deterministic test trace."
  Text.writeFile
    (brainDir </> "walkthrough.md")
    "The fixture contains enough files for analyze-project workflow tests."
  pure projectRoot

interpretAnalyzeProjectStep
  :: AnalyzeProjectTestConfig -> LogicStepNode -> Agentic LlmHistory
interpretAnalyzeProjectStep config step = do
  projectExists <- liftIO (doesDirectoryExist (analyzeProjectRoot config))
  brainExists <- liftIO (doesDirectoryExist (analyzeProjectBrainDir config))
  outputExists <- liftIO (doesDirectoryExist (analyzeProjectOutputDir config))
  liftIO (createDirectoryIfMissing True (analyzeProjectOutputDir config))
  brainEntries <-
    if brainExists
      then fmap (take 20) (liftIO (listDirectory (analyzeProjectBrainDir config)))
      else pure []
  pure
    LlmHistory
      { historyInputs =
          [ InputMessage
              LlmMessage
                { messageId = Just (MessageId ("analyze-project:" <> logicStepIdText step))
                , messageRole = AgentRole "analyze-project-test"
                , messageContent =
                    [ JsonPart
                        ( object
                            [ "step_id" .= logicStepIdText step
                            , "act_type" .= logicStepActType step
                            , "resource_scope" .= logicStepResourceScope step
                            , "resource_target" .= logicStepResourceTarget step
                            , "project_root" .= analyzeProjectRoot config
                            , "brain_dir" .= analyzeProjectBrainDir config
                            , "output_dir" .= analyzeProjectOutputDir config
                            , "project_exists" .= projectExists
                            , "brain_exists" .= brainExists
                            , "output_existed_before_step" .= outputExists
                            , "brain_entries_sample" .= brainEntries
                            ]
                        )
                    ]
                , messagePhase = Nothing
                , messageMetadata = Map.empty
                }
          ]
      , historyResponses = []
      }

historyMentionsProject :: FilePath -> HistorySegment -> Bool
historyMentionsProject projectRoot segment =
  show projectRoot `isInfixOf` show (segmentHistory segment)

logicStepIdText :: LogicStepNode -> Text
logicStepIdText =
  unLogicStepId . logicStepId

emptyAgentState :: AgentState
emptyAgentState =
  AgentState
    { stateRunningSubagents = Map.empty
    , stateHistoryTrace = HistoryTrace []
    , stateOwnHistory = emptyHistory
    , stateActiveTools = Map.empty
    , stateActiveToolCalls = Map.empty
    , stateParallelToolCalls = Map.empty
    , stateCompletedToolOutputs = Map.empty
    , stateNodeStatuses = Map.empty
    , stateEdgeDecisions = []
    , stateReadyNodes = []
    , stateScheduledNodes = []
    , stateProducedArtifacts = []
    , stateLastLlmRequest = Nothing
    , stateLastLlmResponse = Nothing
    , stateBudget =
        BudgetState
          { budgetMaxLlmCalls = Nothing
          , budgetUsedLlmCalls = 0
          , budgetMaxToolCalls = Nothing
          , budgetUsedToolCalls = 0
          , budgetMaxSubagents = Nothing
          , budgetUsedSubagents = 0
          }
    }

runRunnerFixture :: TestLog -> SkillFlowGraph -> IO Bool
runRunnerFixture logFile graph = do
  let sid = SkillId "analyze-project"
      readSummaries =
        AgentNodePointer
          { pointerNode = LogicStepRef sid (LogicStepId "step_read_summaries")
          , pointerAttempt = AttemptId 1
          }
      classifyScope =
        AgentNodePointer
          { pointerNode = LogicStepRef sid (LogicStepId "step_classify_scope_change")
          , pointerAttempt = AttemptId 1
          }
      analyzeRework =
        AgentNodePointer
          { pointerNode = LogicStepRef sid (LogicStepId "step_analyze_rework_shape")
          , pointerAttempt = AttemptId 1
          }
      assignRootCause =
        AgentNodePointer
          { pointerNode = LogicStepRef sid (LogicStepId "step_assign_root_cause")
          , pointerAttempt = AttemptId 1
          }
      target =
        ForkTarget
          { forkTargetNode = pointerNode assignRootCause
          , forkTargetAttempt = AttemptId 1
          }
      env =
        AgentEnv
          { envExecutionGraph = graph
          , envIdentity =
              AgentIdentity
                { identityAgentId = AgentId "runner-fixture-agent"
                , identityRunId = AgentRunId "runner-fixture-run"
                , identityParentRunId = Nothing
                }
          , envCurrentNode = assignRootCause
          , envProvidedHistory = []
          , envHistorySelectionPolicy = SelectReachableDependencies
          , envModel = Nothing
          , envToolRegistry = Map.empty
          }
      state =
        emptyAgentState
          { stateRunningSubagents = Map.empty
          , stateHistoryTrace =
              HistoryTrace
                [ historySegment readSummaries
                , historySegment classifyScope
                , historySegment analyzeRework
                ]
          }
      action = do
        picked <- pickHistory target
        result <- executeNodeTask assignRootCause (pure emptyHistory)
        pure (picked, result)
  result <- runAgentic env state action
  case result of
    Left err -> do
      logSection logFile "runner" ("failed: " <> Text.pack (show err))
      pure False
    Right ((picked, runResult), finalState, events) -> do
      let pickedNodes = pointerNode . segmentNode <$> pickedSegments picked
          finalTrace = unHistoryTrace (stateHistoryTrace finalState)
          ok =
            and
              [ pointerNode readSummaries `elem` pickedNodes
              , pointerNode classifyScope `elem` pickedNodes
              , pointerNode analyzeRework `elem` pickedNodes
              , runResultNode runResult == assignRootCause
              , last finalTrace == historySegment assignRootCause
              , Map.lookup (pointerNode assignRootCause) (stateNodeStatuses finalState)
                  == Just NodeCompleted
              , NodeStarted assignRootCause `elem` events
              , NodeFinished assignRootCause `elem` events
              ]
      logSection
        logFile
        "runner"
        ( "picked_segments="
            <> Text.pack (show (length (pickedSegments picked)))
            <> ", final_trace_segments="
            <> Text.pack (show (length finalTrace))
        )
      pure ok

historySegment :: AgentNodePointer -> HistorySegment
historySegment node =
  HistorySegment
    { segmentNode = node
    , segmentHistory = emptyHistory
    , segmentSummary = Nothing
    }

runProviderTest
  :: Llm backend
  => TestLog
  -> Text
  -> Maybe String
  -> Text
  -> backend
  -> (LlmRequest -> Value)
  -> IO Bool
runProviderTest logFile provider maybeApiKey model backend toProviderJson =
  case maybeApiKey of
    Nothing -> do
      logSection logFile provider ("skipped: missing " <> envName provider)
      putStatus provider "SKIP"
      pure True
    Just _ -> do
      let request = helloWorldRequest model
          providerRequest = toProviderJson request
      logSection logFile provider ("model: " <> model)
      logJson logFile "request" providerRequest
      result <- runLlm backend request
      case result of
        Left err -> do
          logSection logFile provider ("request failed: " <> Text.pack (show err))
          putStatus provider "FAIL"
          pure False
        Right response -> do
          logSection logFile provider ("parsed response: " <> Text.pack (show response))
          let calls = responseToolCalls response
              outputs = executeHelloWorld <$> calls
          logSection logFile provider ("tool calls: " <> Text.pack (show calls))
          logSection logFile provider ("tool execution records: " <> Text.pack (show outputs))
          let ok = not (null calls)
          putStatus provider (if ok then "PASS" else "FAIL")
          pure ok

helloWorldRequest :: Text -> LlmRequest
helloWorldRequest model =
  LlmRequest
    { requestConversationId = Nothing
    , requestTurnId = Nothing
    , requestModel = ModelName model
    , requestInput =
        [ InputMessage
            LlmMessage
              { messageId = Nothing
              , messageRole = UserRole
              , messageContent =
                  [ TextPart
                      "Call the hello_world tool now. Do not answer in text before calling the tool."
                  ]
              , messagePhase = Nothing
              , messageMetadata = Map.empty
              }
        ]
    , requestTools = [helloWorldTool]
    , requestToolChoice = ToolNamed (ToolName "hello_world")
    , requestParams =
        GenerationParams
          { generationTemperature = Just 0
          , generationTopP = Nothing
          , generationMaxOutputTokens = Just 256
          , generationStop = []
          , generationSeed = Nothing
          , generationReasoningEffort = Nothing
          , generationResponseFormat = Nothing
          , generationParallelToolCalls = Just False
          }
    , requestMetadata = Map.empty
    }

helloWorldTool :: ToolSpec
helloWorldTool =
  FunctionTool
    { toolSpecName = ToolName "hello_world"
    , toolSpecDescription = "A test tool. It returns the string hello, world."
    , toolSpecParameters =
        object
          [ "type" .= String "object"
          , "properties" .= object []
          , "required" .= ([] :: [Text])
          ]
    }

executeHelloWorld :: ToolCall -> ToolOutput
executeHelloWorld call =
  ToolOutput
    { toolOutputCallId = toolCallId call
    , toolOutputName = Just (toolCallName call)
    , toolOutputContent = [ToolOutputText "hello, world"]
    , toolOutputSuccess = Just True
    }

responseToolCalls :: LlmResponse -> [ToolCall]
responseToolCalls response =
  [ call
  | ResponseToolCall call <- responseItems response
  ]

envOr :: String -> Text -> IO Text
envOr name fallback =
  maybe fallback Text.pack <$> lookupEnv name

envName :: Text -> Text
envName "openai" = "OPENAI_API_KEY"
envName "gemini" = "GEMINI_API_KEY"
envName provider = Text.toUpper provider <> "_API_KEY"

openTestLog :: IO TestLog
openTestLog = do
  let dir = "test-output"
      path = dir </> "llm-tool-calling.log"
  createDirectoryIfMissing True dir
  handle <- openFile path WriteMode
  pure TestLog {testLogPath = path, testLogHandle = handle}

putStatus :: Text -> Text -> IO ()
putStatus provider status =
  putStrLn ("[" <> Text.unpack provider <> "] " <> Text.unpack status)

logSection :: TestLog -> Text -> Text -> IO ()
logSection logFile provider message =
  Text.hPutStrLn
    (testLogHandle logFile)
    ("[" <> provider <> "] " <> message)

logJson :: TestLog -> Text -> Value -> IO ()
logJson logFile label value = do
  hPutStrLn (testLogHandle logFile) ("--- " <> Text.unpack label <> " ---")
  BL.hPutStr (testLogHandle logFile) (encode value)
  hPutStrLn (testLogHandle logFile) ""
