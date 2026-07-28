module Agent.PickyFork.Llm.Tool
  ( ToolCallId (..)
  , ToolName (..)
  , JsonSchema
  , ToolSpec (..)
  , ToolChoice (..)
  , ToolCallStatus (..)
  , ToolCall (..)
  , ToolOutputContent (..)
  , ToolOutput (..)
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ToolCallId = ToolCallId {unToolCallId :: Text}
  deriving stock (Eq, Ord, Show, Generic)

newtype ToolName = ToolName {unToolName :: Text}
  deriving stock (Eq, Ord, Show, Generic)

type JsonSchema = Value

data ToolSpec
  = FunctionTool
      { toolSpecName :: ToolName
      , toolSpecDescription :: Text
      , toolSpecParameters :: JsonSchema
      }
  | NamespaceTool
      { toolSpecNamespace :: Text
      , toolSpecTools :: [ToolSpec]
      }
  | WebSearchTool
      { toolSpecAllowedDomains :: Maybe [Text]
      }
  | FreeformTool
      { toolSpecName :: ToolName
      , toolSpecDescription :: Text
      , toolSpecInputFormat :: Maybe Text
      }
  deriving stock (Eq, Show, Generic)

data ToolChoice
  = ToolAuto
  | ToolNone
  | ToolRequired
  | ToolNamed ToolName
  deriving stock (Eq, Ord, Show, Generic)

data ToolCallStatus
  = ToolCallQueued
  | ToolCallInProgress
  | ToolCallCompleted
  | ToolCallFailed Text
  deriving stock (Eq, Ord, Show, Generic)

data ToolCall = ToolCall
  { toolCallId :: ToolCallId
  , toolCallName :: ToolName
  , toolCallNamespace :: Maybe Text
  , toolCallArguments :: Value
  , toolCallStatus :: Maybe ToolCallStatus
  }
  deriving stock (Eq, Show, Generic)

data ToolOutputContent
  = ToolOutputText Text
  | ToolOutputImage Text
  | ToolOutputAudio Text
  | ToolOutputJson Value
  | ToolOutputEncrypted Text
  deriving stock (Eq, Show, Generic)

data ToolOutput = ToolOutput
  { toolOutputCallId :: ToolCallId
  , toolOutputName :: Maybe ToolName
  , toolOutputContent :: [ToolOutputContent]
  , toolOutputSuccess :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolCallId
instance FromJSON ToolCallId
instance ToJSON ToolName
instance FromJSON ToolName
instance ToJSON ToolSpec
instance FromJSON ToolSpec
instance ToJSON ToolChoice
instance FromJSON ToolChoice
instance ToJSON ToolCallStatus
instance FromJSON ToolCallStatus
instance ToJSON ToolCall
instance FromJSON ToolCall
instance ToJSON ToolOutputContent
instance FromJSON ToolOutputContent
instance ToJSON ToolOutput
instance FromJSON ToolOutput
