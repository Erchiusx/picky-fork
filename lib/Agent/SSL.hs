module Agent.SSL
  ( -- * Common types
    SkillGraph
  , SkillFlowGraph (..)
  , SslSkill (..)
  , SslSkillRecord (..)
  , ParsedSkill (..)
  , ParsedScene (..)
  , ParsedLogicStep (..)
  , NextRule (..)
  , SslLoadError (..)
  , SkillId (..)
  , SceneId (..)
  , LogicStepId (..)
  , NodeKey (..)
  , GraphNodeRef (..)
  , EdgeKind (..)
  , SkillEdge (..)
  , SkillNode (..)
  , SceneNode (..)
  , LogicStepNode (..)

    -- * Main parsing interface
  , parseSslSkillFile
  , parseSslSkillFileOrFail
  , parseSslSkills
  , parseSslSkillsOrFail

    -- * Lower-level record and corpus graph parsing
  , parseSslRecordFile
  , parseSslRecordFileOrFail
  , parseSslRecords
  , parseSslRecordsOrFail
  , parseSslCorpusGraphFile
  , parseSslCorpusGraphFileOrFail
  , parseSslCorpusGraph
  , parseSslCorpusGraphOrFail
  , buildSslSkill
  , buildSslSkills
  , buildSkillGraph

    -- * Small query helpers
  , graphSkillCount
  , graphSceneCount
  , graphLogicStepCount
  , graphEdgeCount
  , graphEdgesByKind
  , graphEdgeCountsByKind
  , skillSceneCount
  , skillLogicStepCount
  , skillEdgeCount
  , skillEdgesByKind
  , skillEdgeCountsByKind
  )
where

import Agent.SSL.Internal
import Control.Exception (throwIO)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

parseSslSkillFile :: FilePath -> IO (Either [SslLoadError] [SslSkill])
parseSslSkillFile path = fmap buildSslSkills <$> parseSslRecordFile path

parseSslSkillFileOrFail :: FilePath -> IO [SslSkill]
parseSslSkillFileOrFail path = buildSslSkills <$> parseSslRecordFileOrFail path

parseSslSkills :: FilePath -> IO (Either [SslLoadError] [SslSkill])
parseSslSkills sslRoot = fmap buildSslSkills <$> parseSslRecords sslRoot

parseSslSkillsOrFail :: FilePath -> IO [SslSkill]
parseSslSkillsOrFail sslRoot = buildSslSkills <$> parseSslRecordsOrFail sslRoot

parseSslRecordFile :: FilePath -> IO (Either [SslLoadError] [SslSkillRecord])
parseSslRecordFile = loadSslJsonlFile

parseSslRecordFileOrFail :: FilePath -> IO [SslSkillRecord]
parseSslRecordFileOrFail path =
  either (throwIO . userError . formatLoadErrors) pure =<< parseSslRecordFile path

parseSslRecords :: FilePath -> IO (Either [SslLoadError] [SslSkillRecord])
parseSslRecords sslRoot = do
  files <- annotatedCorpusFiles sslRoot
  loaded <- traverse loadSslJsonlFile files
  let
    errors = concat [err | Left err <- loaded]
    records = concat [xs | Right xs <- loaded]
  pure $
    if null errors
      then Right records
      else Left errors

parseSslRecordsOrFail :: FilePath -> IO [SslSkillRecord]
parseSslRecordsOrFail sslRoot =
  either (throwIO . userError . formatLoadErrors) pure =<< parseSslRecords sslRoot

parseSslCorpusGraphFile :: FilePath -> IO (Either [SslLoadError] SkillGraph)
parseSslCorpusGraphFile path = fmap buildSkillGraph <$> parseSslRecordFile path

parseSslCorpusGraphFileOrFail :: FilePath -> IO SkillGraph
parseSslCorpusGraphFileOrFail path = buildSkillGraph <$> parseSslRecordFileOrFail path

parseSslCorpusGraph :: FilePath -> IO (Either [SslLoadError] SkillGraph)
parseSslCorpusGraph = loadSslDirectory

parseSslCorpusGraphOrFail :: FilePath -> IO SkillGraph
parseSslCorpusGraphOrFail sslRoot =
  either (throwIO . userError . formatLoadErrors) pure
    =<< parseSslCorpusGraph sslRoot

graphSkillCount :: SkillGraph -> Int
graphSkillCount = Map.size . graphSkills

graphSceneCount :: SkillGraph -> Int
graphSceneCount = Map.size . graphScenes

graphLogicStepCount :: SkillGraph -> Int
graphLogicStepCount = Map.size . graphLogicSteps

graphEdgeCount :: SkillGraph -> Int
graphEdgeCount = length . graphEdges

graphEdgesByKind :: SkillGraph -> Map EdgeKind [SkillEdge]
graphEdgesByKind = edgesByKind . graphEdges

graphEdgeCountsByKind :: SkillGraph -> Map EdgeKind Int
graphEdgeCountsByKind = edgeCountsByKind . graphEdges

skillSceneCount :: SslSkill -> Int
skillSceneCount = Map.size . flowScenes . sslSkillGraph

skillLogicStepCount :: SslSkill -> Int
skillLogicStepCount = Map.size . flowLogicSteps . sslSkillGraph

skillEdgeCount :: SslSkill -> Int
skillEdgeCount = length . flowEdges . sslSkillGraph

skillEdgesByKind :: SslSkill -> Map EdgeKind [SkillEdge]
skillEdgesByKind = edgesByKind . flowEdges . sslSkillGraph

skillEdgeCountsByKind :: SslSkill -> Map EdgeKind Int
skillEdgeCountsByKind = edgeCountsByKind . flowEdges . sslSkillGraph

edgesByKind :: [SkillEdge] -> Map EdgeKind [SkillEdge]
edgesByKind =
  foldl' insertEdge Map.empty
 where
  insertEdge acc edge =
    Map.insertWith (<>) (edgeKind edge) [edge] acc

edgeCountsByKind :: [SkillEdge] -> Map EdgeKind Int
edgeCountsByKind =
  fmap length . edgesByKind

formatLoadErrors :: [SslLoadError] -> String
formatLoadErrors [] = "unknown SSL load error"
formatLoadErrors errors =
  unlines (formatLoadError <$> take 20 errors)
    <> if length errors > 20
      then "... and " <> show (length errors - 20) <> " more SSL load errors"
      else ""

formatLoadError :: SslLoadError -> String
formatLoadError err =
  loadErrorFile err
    <> ":"
    <> show (loadErrorLine err)
    <> ": "
    <> loadErrorMessage err
