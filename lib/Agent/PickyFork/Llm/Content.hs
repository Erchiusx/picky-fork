module Agent.PickyFork.Llm.Content
  ( MessageId (..)
  , Role (..)
  , MessagePhase (..)
  , ImageDetail (..)
  , MediaRef (..)
  , ContentPart (..)
  , LlmMessage (..)
  , LlmInputItem (..)
  , LlmResponseItem (..)
  , LlmHistory (..)
  , emptyHistory
  , appendInputItem
  , appendResponseItem
  , historyInputItems
  , combineHistories
  )
where

import Agent.PickyFork.Llm.Tool (ToolCall, ToolOutput)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype MessageId = MessageId {unMessageId :: Text}
  deriving stock (Eq, Ord, Show, Generic)

data Role
  = SystemRole
  | DeveloperRole
  | UserRole
  | AssistantRole
  | ToolRole
  | AgentRole Text
  deriving stock (Eq, Ord, Show, Generic)

data MessagePhase
  = CommentaryPhase
  | FinalAnswerPhase
  | UnknownPhase
  deriving stock (Eq, Ord, Show, Generic)

data ImageDetail
  = ImageAuto
  | ImageLow
  | ImageHigh
  | ImageOriginal
  deriving stock (Eq, Ord, Show, Generic)

data MediaRef
  = MediaUrl Text
  | MediaFile FilePath
  | MediaDataUrl Text
  | MediaBytes
      { mediaMimeType :: Text
      , mediaBytesBase64 :: Text
      }
  deriving stock (Eq, Ord, Show, Generic)

data ContentPart
  = TextPart Text
  | LinkPart Text
  | ImagePart MediaRef (Maybe ImageDetail)
  | AudioPart MediaRef
  | FilePart MediaRef
  | JsonPart Value
  | ToolCallPart ToolCall
  | ToolOutputPart ToolOutput
  | RefusalPart Text
  deriving stock (Eq, Show, Generic)

data LlmMessage = LlmMessage
  { messageId :: Maybe MessageId
  , messageRole :: Role
  , messageContent :: [ContentPart]
  , messagePhase :: Maybe MessagePhase
  , messageMetadata :: Map Text Value
  }
  deriving stock (Eq, Show, Generic)

data LlmInputItem
  = InputMessage LlmMessage
  | InputToolOutput ToolOutput
  | InputResponse LlmResponseItem
  deriving stock (Eq, Show, Generic)

data LlmResponseItem
  = ResponseMessage LlmMessage
  | ResponseReasoning
      { reasoningSummary :: [Text]
      , reasoningEncryptedContent :: Maybe Text
      }
  | ResponseToolCall ToolCall
  | ResponseToolOutput ToolOutput
  | ResponseOther Value
  deriving stock (Eq, Show, Generic)

data LlmHistory = LlmHistory
  { historyInputs :: [LlmInputItem]
  , historyResponses :: [LlmResponseItem]
  }
  deriving stock (Eq, Show, Generic)

emptyHistory :: LlmHistory
emptyHistory =
  LlmHistory
    { historyInputs = []
    , historyResponses = []
    }

appendInputItem :: LlmInputItem -> LlmHistory -> LlmHistory
appendInputItem item history =
  history{historyInputs = historyInputs history <> [item]}

appendResponseItem :: LlmResponseItem -> LlmHistory -> LlmHistory
appendResponseItem item history =
  history{historyResponses = historyResponses history <> [item]}

-- Completed turns live in historyInputs in chronological order. The response
-- field is an optional, not-yet-replayed tail, never a second copy of the log.
historyInputItems :: LlmHistory -> [LlmInputItem]
historyInputItems history =
  historyInputs history <> map InputResponse (historyResponses history)

combineHistories :: [LlmHistory] -> LlmHistory
combineHistories histories = LlmHistory (concatMap historyInputItems histories) []

instance ToJSON MessageId
instance FromJSON MessageId
instance ToJSON Role
instance FromJSON Role
instance ToJSON MessagePhase
instance FromJSON MessagePhase
instance ToJSON ImageDetail
instance FromJSON ImageDetail
instance ToJSON MediaRef
instance FromJSON MediaRef
instance ToJSON ContentPart
instance FromJSON ContentPart
instance ToJSON LlmMessage
instance FromJSON LlmMessage
instance ToJSON LlmInputItem
instance FromJSON LlmInputItem
instance ToJSON LlmResponseItem
instance FromJSON LlmResponseItem
instance ToJSON LlmHistory
instance FromJSON LlmHistory
