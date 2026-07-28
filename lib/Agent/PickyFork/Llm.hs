module Agent.PickyFork.Llm
  ( ConversationId (..)
  , TurnId (..)
  , ModelName (..)
  , GenerationParams (..)
  , ReasoningEffort (..)
  , ResponseFormat (..)
  , LlmRequest (..)
  , LlmResponse (..)
  , LlmError (..)
  , Llm (..)
  , runLlmAsync
  , module Agent.PickyFork.Llm.Content
  , module Agent.PickyFork.Llm.Tool
  )
where

import Agent.PickyFork.Llm.Content
import Agent.PickyFork.Llm.Tool
import Control.Concurrent.Async (Async, async)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ConversationId = ConversationId {unConversationId :: Text}
  deriving stock (Eq, Ord, Show, Generic)

newtype TurnId = TurnId {unTurnId :: Text}
  deriving stock (Eq, Ord, Show, Generic)

newtype ModelName = ModelName {unModelName :: Text}
  deriving stock (Eq, Ord, Show, Generic)

data GenerationParams = GenerationParams
  { generationTemperature :: Maybe Double
  , generationTopP :: Maybe Double
  , generationMaxOutputTokens :: Maybe Int
  , generationStop :: [Text]
  , generationSeed :: Maybe Int
  , generationReasoningEffort :: Maybe ReasoningEffort
  , generationResponseFormat :: Maybe ResponseFormat
  , generationParallelToolCalls :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)

data ReasoningEffort
  = ReasoningLow
  | ReasoningMedium
  | ReasoningHigh
  deriving stock (Eq, Ord, Show, Generic)

data ResponseFormat
  = ResponseText
  | ResponseJsonObject
  | ResponseJsonSchema Value
  deriving stock (Eq, Show, Generic)

data LlmRequest = LlmRequest
  { requestConversationId :: Maybe ConversationId
  , requestTurnId :: Maybe TurnId
  , requestModel :: ModelName
  , requestInput :: [LlmInputItem]
  , requestTools :: [ToolSpec]
  , requestToolChoice :: ToolChoice
  , requestParams :: GenerationParams
  , requestMetadata :: Map Text Value
  }
  deriving stock (Eq, Show, Generic)

data LlmResponse = LlmResponse
  { responseItems :: [LlmResponseItem]
  }
  deriving stock (Eq, Show, Generic)

data LlmError
  = LlmHttpError Int Text
  | LlmDecodeError Text
  | LlmProviderError Text
  | LlmRateLimited (Maybe Int) Text
  | LlmTimeout Text
  | LlmMissingApiKey
  | LlmUnsupportedFeature Text
  deriving stock (Eq, Ord, Show, Generic)

class Llm backend where
  llmName :: backend -> Text
  runLlm :: backend -> LlmRequest -> IO (Either LlmError LlmResponse)

runLlmAsync
  :: Llm backend => backend -> LlmRequest -> IO (Async (Either LlmError LlmResponse))
runLlmAsync backend request =
  async (runLlm backend request)

instance ToJSON ConversationId
instance FromJSON ConversationId
instance ToJSON TurnId
instance FromJSON TurnId
instance ToJSON ModelName
instance FromJSON ModelName
instance ToJSON GenerationParams
instance FromJSON GenerationParams
instance ToJSON ReasoningEffort
instance FromJSON ReasoningEffort
instance ToJSON ResponseFormat
instance FromJSON ResponseFormat
instance ToJSON LlmRequest
instance FromJSON LlmRequest
instance ToJSON LlmResponse
instance FromJSON LlmResponse
instance ToJSON LlmError
instance FromJSON LlmError
