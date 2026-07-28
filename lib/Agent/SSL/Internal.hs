module Agent.SSL.Internal
  ( SkillId (..)
  , SceneId (..)
  , LogicStepId (..)
  , NodeKey (..)
  , GraphNodeRef (..)
  , EdgeKind (..)
  , SkillEdge (..)
  , SkillNode (..)
  , SceneNode (..)
  , LogicStepNode (..)
  , SkillFlowGraph (..)
  , SslSkill (..)
  , SkillGraph (..)
  , SslSkillRecord (..)
  , ParsedSkill (..)
  , ParsedScene (..)
  , ParsedLogicStep (..)
  , NextRule (..)
  , SslLoadError (..)
  , emptySkillGraph
  , buildSslSkill
  , buildSslSkills
  , buildSkillGraph
  , loadSslDirectory
  , loadSslJsonlFile
  , annotatedCorpusFiles
  , skillNodeKey
  , sceneNodeKey
  , logicStepNodeKey
  )
where

import Data.Aeson
  ( FromJSON (parseJSON)
  , Value (..)
  , eitherDecodeStrict'
  , withObject
  , (.!=)
  , (.:)
  , (.:?)
  )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

newtype SkillId = SkillId {unSkillId :: Text}
  deriving stock (Eq, Ord, Show)

newtype SceneId = SceneId {unSceneId :: Text}
  deriving stock (Eq, Ord, Show)

newtype LogicStepId = LogicStepId {unLogicStepId :: Text}
  deriving stock (Eq, Ord, Show)

newtype NodeKey = NodeKey {unNodeKey :: Text}
  deriving stock (Eq, Ord, Show)

data GraphNodeRef
  = SkillRef SkillId
  | SceneRef SkillId SceneId
  | LogicStepRef SkillId LogicStepId
  | TerminalRef SkillId Text
  deriving stock (Eq, Ord, Show)

data EdgeKind
  = SkillEntryScene
  | SkillSubscene
  | SceneContainsLogicStep
  | SceneNext
  | LogicStepNext
  deriving stock (Eq, Ord, Show)

data SkillEdge = SkillEdge
  { edgeFrom :: GraphNodeRef
  , edgeTo :: GraphNodeRef
  , edgeKind :: EdgeKind
  , edgeCondition :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)

data SkillNode = SkillNode
  { skillId :: SkillId
  , skillSlug :: Text
  , skillName :: Text
  , skillGoal :: Maybe Text
  , skillTopPattern :: Maybe Text
  , skillTags :: [Text]
  , skillEntrySceneId :: Maybe SceneId
  , skillSubscenes :: [SceneId]
  , skillRawMarkdown :: Maybe Text
  , skillMetadataJson :: Value
  , skillSslRecordJson :: Value
  }
  deriving stock (Eq, Show)

data SceneNode = SceneNode
  { sceneOwner :: SkillId
  , sceneId :: SceneId
  , sceneName :: Maybe Text
  , sceneGoal :: Maybe Text
  , sceneType :: Maybe Text
  , sceneEntryLogicStepId :: Maybe LogicStepId
  , sceneContainedLogicSteps :: [LogicStepId]
  , sceneRawJson :: Value
  }
  deriving stock (Eq, Show)

data LogicStepNode = LogicStepNode
  { logicStepOwner :: SkillId
  , logicStepId :: LogicStepId
  , logicStepActType :: Maybe Text
  , logicStepActor :: Maybe Text
  , logicStepObject :: Maybe Text
  , logicStepInstrument :: Maybe Text
  , logicStepResourceScope :: Maybe Text
  , logicStepResourceTarget :: Maybe Text
  , logicStepRawJson :: Value
  }
  deriving stock (Eq, Show)

data SkillFlowGraph = SkillFlowGraph
  { flowSkill :: SkillNode
  , flowScenes :: Map SceneId SceneNode
  , flowLogicSteps :: Map LogicStepId LogicStepNode
  , flowEdges :: [SkillEdge]
  }
  deriving stock (Eq, Show)

data SslSkill = SslSkill
  { sslSkillRecord :: SslSkillRecord
  , sslSkillGraph :: SkillFlowGraph
  }
  deriving stock (Eq, Show)

data SkillGraph = SkillGraph
  { graphSkills :: Map NodeKey SkillNode
  , graphScenes :: Map NodeKey SceneNode
  , graphLogicSteps :: Map NodeKey LogicStepNode
  , graphEdges :: [SkillEdge]
  }
  deriving stock (Eq, Show)

data SslSkillRecord = SslSkillRecord
  { recordSlug :: Text
  , recordMetadataJson :: Value
  , recordRawSkillMarkdown :: Maybe Text
  , recordSslRecordJson :: Value
  , recordSkill :: ParsedSkill
  , recordScenes :: [ParsedScene]
  , recordLogicSteps :: [ParsedLogicStep]
  }
  deriving stock (Eq, Show)

data ParsedSkill = ParsedSkill
  { parsedSkillId :: SkillId
  , parsedSkillName :: Text
  , parsedSkillGoal :: Maybe Text
  , parsedSkillTopPattern :: Maybe Text
  , parsedSkillTags :: [Text]
  , parsedSkillEntrySceneId :: Maybe SceneId
  , parsedSkillSubscenes :: [SceneId]
  }
  deriving stock (Eq, Show)

data ParsedScene = ParsedScene
  { parsedSceneId :: SceneId
  , parsedSceneName :: Maybe Text
  , parsedSceneGoal :: Maybe Text
  , parsedSceneType :: Maybe Text
  , parsedSceneEntryLogicStepId :: Maybe LogicStepId
  , parsedSceneContainedLogicSteps :: [LogicStepId]
  , parsedSceneNextRules :: [NextRule]
  , parsedSceneRawJson :: Value
  }
  deriving stock (Eq, Show)

data ParsedLogicStep = ParsedLogicStep
  { parsedLogicStepId :: LogicStepId
  , parsedLogicStepActType :: Maybe Text
  , parsedLogicStepActor :: Maybe Text
  , parsedLogicStepObject :: Maybe Text
  , parsedLogicStepInstrument :: Maybe Text
  , parsedLogicStepResourceScope :: Maybe Text
  , parsedLogicStepResourceTarget :: Maybe Text
  , parsedLogicStepNextRules :: [NextRule]
  , parsedLogicStepRawJson :: Value
  }
  deriving stock (Eq, Show)

data NextRule = NextRule
  { nextRuleCondition :: Maybe Text
  , nextRuleTarget :: Text
  }
  deriving stock (Eq, Show)

data SslLoadError = SslLoadError
  { loadErrorFile :: FilePath
  , loadErrorLine :: Int
  , loadErrorMessage :: String
  }
  deriving stock (Eq, Show)

instance FromJSON SslSkillRecord where
  parseJSON =
    withObject "SslSkillRecord" $ \o -> do
      slug <- o .: "slug"
      metadata <- o .:? "metadata" .!= Object KeyMap.empty
      rawSkillMd <- o .:? "raw_skill_md"
      sslRecord <- o .: "ssl_record"
      case sslRecord of
        Object ssl -> do
          skill <- ssl .: "skill"
          scenes <- ssl .:? "scenes" .!= []
          logicSteps <- ssl .:? "logic_steps" .!= []
          pure
            SslSkillRecord
              { recordSlug = slug
              , recordMetadataJson = metadata
              , recordRawSkillMarkdown = rawSkillMd
              , recordSslRecordJson = sslRecord
              , recordSkill = skill
              , recordScenes = scenes
              , recordLogicSteps = logicSteps
              }
        _ -> fail "ssl_record must be an object"

instance FromJSON ParsedSkill where
  parseJSON =
    withObject "ParsedSkill" $ \o ->
      (ParsedSkill . SkillId <$> (o .: "skill_id"))
        <*> o .: "skill_name"
        <*> o .:? "skill_goal"
        <*> o .:? "top_pattern"
        <*> o .:? "tags" .!= []
        <*> (fmap SceneId <$> o .:? "entry_scene_id")
        <*> (fmap SceneId <$> o .:? "subscenes" .!= [])

instance FromJSON ParsedScene where
  parseJSON value =
    withObject
      "ParsedScene"
      ( \o ->
          (ParsedScene . SceneId <$> (o .: "scene_id"))
            <*> o .:? "scene_name"
            <*> o .:? "scene_goal"
            <*> o .:? "scene_type"
            <*> (fmap LogicStepId <$> o .:? "entry_logic_step_id")
            <*> (fmap LogicStepId <$> o .:? "contained_logic_steps" .!= [])
            <*> o .:? "next_scene_rules" .!= []
            <*> pure value
      )
      value

instance FromJSON ParsedLogicStep where
  parseJSON value =
    withObject
      "ParsedLogicStep"
      ( \o ->
          ParsedLogicStep
            <$> (LogicStepId <$> o .: "logic_step_id")
            <*> o .:? "act_type"
            <*> o .:? "actor"
            <*> o .:? "object"
            <*> o .:? "instrument"
            <*> o .:? "resource_scope"
            <*> o .:? "resource_target"
            <*> o .:? "next_step_rules" .!= []
            <*> pure value
      )
      value

instance FromJSON NextRule where
  parseJSON =
    withObject "NextRule" $ \o ->
      NextRule
        <$> o .:? "condition"
        <*> o .: "target"

emptySkillGraph :: SkillGraph
emptySkillGraph =
  SkillGraph
    { graphSkills = Map.empty
    , graphScenes = Map.empty
    , graphLogicSteps = Map.empty
    , graphEdges = []
    }

buildSkillGraph :: [SslSkillRecord] -> SkillGraph
buildSkillGraph = foldl' insertRecord emptySkillGraph

buildSslSkills :: [SslSkillRecord] -> [SslSkill]
buildSslSkills = fmap buildSslSkill

buildSslSkill :: SslSkillRecord -> SslSkill
buildSslSkill record =
  SslSkill
    { sslSkillRecord = record
    , sslSkillGraph =
        SkillFlowGraph
          { flowSkill = skillNode'
          , flowScenes = Map.fromList [(sceneId node, node) | node <- sceneNodes]
          , flowLogicSteps =
              Map.fromList [(logicStepId node, node) | node <- logicStepNodes]
          , flowEdges = recordEdges record
          }
    }
 where
  sid = parsedSkillId (recordSkill record)
  skillNode' = recordSkillNode record
  sceneNodes = sceneNode sid <$> recordScenes record
  logicStepNodes = logicStepNode sid <$> recordLogicSteps record

loadSslDirectory :: FilePath -> IO (Either [SslLoadError] SkillGraph)
loadSslDirectory sslRoot = do
  files <- annotatedCorpusFiles sslRoot
  loaded <- traverse loadSslJsonlFile files
  let
    errors = concat [err | Left err <- loaded]
    records = concat [xs | Right xs <- loaded]
  pure $
    if null errors
      then Right (buildSkillGraph records)
      else Left errors

loadSslJsonlFile :: FilePath -> IO (Either [SslLoadError] [SslSkillRecord])
loadSslJsonlFile path = do
  content <- BS.readFile path
  let
    decoded = zipWith (decodeLine path) [1 ..] (BSC.lines content)
    errors = [err | Left err <- decoded]
    records = [record | Right record <- decoded]
  pure $
    if null errors
      then Right records
      else Left errors

skillNodeKey :: SkillId -> NodeKey
skillNodeKey (SkillId sid) = NodeKey ("skill:" <> sid)

sceneNodeKey :: SkillId -> SceneId -> NodeKey
sceneNodeKey (SkillId sid) (SceneId sceneId') =
  NodeKey ("scene:" <> sid <> ":" <> sceneId')

logicStepNodeKey :: SkillId -> LogicStepId -> NodeKey
logicStepNodeKey (SkillId sid) (LogicStepId logicStepId') =
  NodeKey ("logic:" <> sid <> ":" <> logicStepId')

insertRecord :: SkillGraph -> SslSkillRecord -> SkillGraph
insertRecord graph record =
  graph
    { graphSkills = Map.insert (skillNodeKey sid) skillNode' (graphSkills graph)
    , graphScenes = foldl' insertScene (graphScenes graph) sceneNodes
    , graphLogicSteps = foldl' insertLogicStep (graphLogicSteps graph) logicStepNodes
    , graphEdges = graphEdges graph <> edges
    }
 where
  sid = parsedSkillId (recordSkill record)
  skillNode' = recordSkillNode record
  sceneNodes = sceneNode sid <$> recordScenes record
  logicStepNodes = logicStepNode sid <$> recordLogicSteps record
  insertScene acc node = Map.insert (sceneNodeKey sid (sceneId node)) node acc
  insertLogicStep acc node = Map.insert (logicStepNodeKey sid (logicStepId node)) node acc
  edges = recordEdges record

recordSkillNode :: SslSkillRecord -> SkillNode
recordSkillNode record =
  SkillNode
    { skillId = sid
    , skillSlug = recordSlug record
    , skillName = parsedSkillName skill
    , skillGoal = parsedSkillGoal skill
    , skillTopPattern = parsedSkillTopPattern skill
    , skillTags = parsedSkillTags skill
    , skillEntrySceneId = parsedSkillEntrySceneId skill
    , skillSubscenes = parsedSkillSubscenes skill
    , skillRawMarkdown = recordRawSkillMarkdown record
    , skillMetadataJson = recordMetadataJson record
    , skillSslRecordJson = recordSslRecordJson record
    }
 where
  skill = recordSkill record
  sid = parsedSkillId skill

recordEdges :: SslSkillRecord -> [SkillEdge]
recordEdges record =
  uniqueEdges $
    maybe [] (\entry -> [entrySceneEdge sid entry]) (parsedSkillEntrySceneId skill)
      <> (subsceneEdge sid <$> parsedSkillSubscenes skill)
      <> concatMap (sceneEdges sid) (recordScenes record)
      <> concatMap (logicStepEdges sid) (recordLogicSteps record)
 where
  skill = recordSkill record
  sid = parsedSkillId skill

uniqueEdges :: [SkillEdge] -> [SkillEdge]
uniqueEdges = go Set.empty
 where
  go _ [] = []
  go seen (edge : rest)
    | Set.member edge seen = go seen rest
    | otherwise = edge : go (Set.insert edge seen) rest

sceneNode :: SkillId -> ParsedScene -> SceneNode
sceneNode sid scene =
  SceneNode
    { sceneOwner = sid
    , sceneId = parsedSceneId scene
    , sceneName = parsedSceneName scene
    , sceneGoal = parsedSceneGoal scene
    , sceneType = parsedSceneType scene
    , sceneEntryLogicStepId = parsedSceneEntryLogicStepId scene
    , sceneContainedLogicSteps = parsedSceneContainedLogicSteps scene
    , sceneRawJson = parsedSceneRawJson scene
    }

logicStepNode :: SkillId -> ParsedLogicStep -> LogicStepNode
logicStepNode sid step =
  LogicStepNode
    { logicStepOwner = sid
    , logicStepId = parsedLogicStepId step
    , logicStepActType = parsedLogicStepActType step
    , logicStepActor = parsedLogicStepActor step
    , logicStepObject = parsedLogicStepObject step
    , logicStepInstrument = parsedLogicStepInstrument step
    , logicStepResourceScope = parsedLogicStepResourceScope step
    , logicStepResourceTarget = parsedLogicStepResourceTarget step
    , logicStepRawJson = parsedLogicStepRawJson step
    }

entrySceneEdge :: SkillId -> SceneId -> SkillEdge
entrySceneEdge sid sceneId' =
  SkillEdge (SkillRef sid) (SceneRef sid sceneId') SkillEntryScene Nothing

subsceneEdge :: SkillId -> SceneId -> SkillEdge
subsceneEdge sid sceneId' =
  SkillEdge (SkillRef sid) (SceneRef sid sceneId') SkillSubscene Nothing

sceneEdges :: SkillId -> ParsedScene -> [SkillEdge]
sceneEdges sid scene =
  entryLogicStepEdges <> containedStepEdges <> nextSceneEdges
 where
  from = SceneRef sid (parsedSceneId scene)
  entryLogicStepEdges =
    maybe
      []
      ( \stepId -> [SkillEdge from (LogicStepRef sid stepId) SceneContainsLogicStep Nothing]
      )
      (parsedSceneEntryLogicStepId scene)
  containedStepEdges =
    [ SkillEdge from (LogicStepRef sid stepId) SceneContainsLogicStep Nothing
    | stepId <- parsedSceneContainedLogicSteps scene
    ]
  nextSceneEdges =
    [ SkillEdge from (sceneTarget sid rule) SceneNext (nextRuleCondition rule)
    | rule <- parsedSceneNextRules scene
    ]

logicStepEdges :: SkillId -> ParsedLogicStep -> [SkillEdge]
logicStepEdges sid step =
  [ SkillEdge from (logicStepTarget sid rule) LogicStepNext (nextRuleCondition rule)
  | rule <- parsedLogicStepNextRules step
  ]
 where
  from = LogicStepRef sid (parsedLogicStepId step)

sceneTarget :: SkillId -> NextRule -> GraphNodeRef
sceneTarget sid rule =
  case nextRuleTarget rule of
    "END_SUCCESS" -> TerminalRef sid "END_SUCCESS"
    "END_FAIL" -> TerminalRef sid "END_FAIL"
    target -> SceneRef sid (SceneId target)

logicStepTarget :: SkillId -> NextRule -> GraphNodeRef
logicStepTarget sid rule =
  case nextRuleTarget rule of
    "YIELD_SUCCESS" -> TerminalRef sid "YIELD_SUCCESS"
    "YIELD_FAIL" -> TerminalRef sid "YIELD_FAIL"
    "END_SUCCESS" -> TerminalRef sid "END_SUCCESS"
    "END_FAIL" -> TerminalRef sid "END_FAIL"
    target -> LogicStepRef sid (LogicStepId target)

decodeLine
  :: FilePath -> Int -> BS.ByteString -> Either SslLoadError SslSkillRecord
decodeLine path lineNo rawLine
  | BS.null rawLine = Left (SslLoadError path lineNo "empty JSONL line")
  | otherwise =
      case eitherDecodeStrict' rawLine of
        Left message -> Left (SslLoadError path lineNo message)
        Right record -> Right record

annotatedCorpusFiles :: FilePath -> IO [FilePath]
annotatedCorpusFiles sslRoot = do
  let dir = sslRoot </> "data" </> "annotated_skill_corpus"
  exists <- doesDirectoryExist dir
  if exists
    then do
      names <- listDirectory dir
      pure
        [ dir </> name
        | name <- names
        , let textName = Text.pack name
        , "annotated_skill_corpus-" `Text.isPrefixOf` textName
        , ".jsonl" `Text.isSuffixOf` textName
        ]
    else pure []
