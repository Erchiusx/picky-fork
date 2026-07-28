module Agent.PickyFork.Llm.Transport.Http
  ( HttpTransport (..)
  , newHttpTransport
  )
where

import Agent.PickyFork.Llm (LlmError (..))
import Agent.PickyFork.Llm.Transport
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , Response (..)
  , httpLbs
  , method
  , newManager
  , parseRequest
  , requestBody
  , requestHeaders
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (HeaderName, hContentType)
import Network.HTTP.Types.Status (statusCode)

newtype HttpTransport = HttpTransport
  { httpTransportManager :: Manager
  }

newHttpTransport :: IO HttpTransport
newHttpTransport = HttpTransport <$> newManager tlsManagerSettings

instance LlmTransport HttpTransport where
  sendJsonRequest transport request = do
    baseRequest <- parseRequest (Text.unpack (httpUrl request))
    let outbound =
          baseRequest
            { method = methodBytes (httpMethod request)
            , requestHeaders = headersBytes (httpHeaders request)
            , requestBody = maybe mempty (RequestBodyLBS . encode) (httpJsonBody request)
            }
    response <- httpLbs outbound (httpTransportManager transport)
    let body = responseBody response
    pure $
      case eitherDecode body of
        Left err ->
          Left
            (LlmDecodeError (Text.pack err <> ": " <> Text.decodeUtf8 (BL.toStrict body)))
        Right value ->
          Right
            HttpResponse
              { httpStatus = statusCode (responseStatus response)
              , httpJson = value
              }

methodBytes :: HttpMethod -> BSC.ByteString
methodBytes HttpGet = "GET"
methodBytes HttpPost = "POST"

headersBytes :: [(Text.Text, Text.Text)] -> [(HeaderName, BSC.ByteString)]
headersBytes headers =
  ensureContentType
    [ (CI.mk (Text.encodeUtf8 name), Text.encodeUtf8 value)
    | (name, value) <- headers
    ]

ensureContentType
  :: [(HeaderName, BSC.ByteString)] -> [(HeaderName, BSC.ByteString)]
ensureContentType headers
  | any ((== hContentType) . fst) headers = headers
  | otherwise = (hContentType, "application/json") : headers
