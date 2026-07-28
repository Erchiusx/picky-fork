module Agent.PickyFork.Llm.Transport
  ( HttpMethod (..)
  , HttpRequest (..)
  , HttpResponse (..)
  , LlmTransport (..)
  )
where

import Agent.PickyFork.Llm (LlmError)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)

data HttpMethod
  = HttpGet
  | HttpPost
  deriving stock (Eq, Ord, Show, Generic)

data HttpRequest = HttpRequest
  { httpMethod :: HttpMethod
  , httpUrl :: Text
  , httpHeaders :: [(Text, Text)]
  , httpJsonBody :: Maybe Value
  }
  deriving stock (Eq, Show, Generic)

data HttpResponse = HttpResponse
  { httpStatus :: Int
  , httpJson :: Value
  }
  deriving stock (Eq, Show, Generic)

class LlmTransport transport where
  sendJsonRequest :: transport -> HttpRequest -> IO (Either LlmError HttpResponse)

instance ToJSON HttpMethod
instance FromJSON HttpMethod
instance ToJSON HttpRequest
instance FromJSON HttpRequest
instance ToJSON HttpResponse
instance FromJSON HttpResponse
