module Agent.Backends.GPT
  ( GPT (..)
  , defaultGPTBaseUrl
  , gptRequestJson
  , gptResponseFromJson
  )
where

import Agent.PickyFork.Llm
import Agent.PickyFork.Llm.Transport
import Control.Applicative ((<|>))
import Data.Aeson
  ( Value (..)
  , decodeStrict'
  , encode
  , object
  , withArray
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

data GPT transport = GPT
  { gptApiKey :: Text
  , gptBaseUrl :: Text
  , gptTransport :: transport
  }

defaultGPTBaseUrl :: Text
defaultGPTBaseUrl = "https://api.openai.com/v1/responses"

gptRequestJson :: LlmRequest -> Value
gptRequestJson request =
  object
    ( [ "model" .= unModelName (requestModel request)
      , "input" .= fmap gptInputItem (requestInput request)
      , "tool_choice" .= gptToolChoice (requestToolChoice request)
      ]
        <> [ "tools" .= tools
           | let tools = mapMaybeTool gptToolSpec (requestTools request)
           , not (null tools)
           ]
    )

gptResponseFromJson :: Value -> LlmResponse
gptResponseFromJson raw =
  LlmResponse
    { responseItems = fromMaybeOther raw (parseMaybe parseOutput raw)
    }

instance LlmTransport transport => Llm (GPT transport) where
  llmName _ = "gpt"

  runLlm backend request = do
    result <-
      sendJsonRequest
        (gptTransport backend)
        HttpRequest
          { httpMethod = HttpPost
          , httpUrl = gptBaseUrl backend
          , httpHeaders =
              [ ("Authorization", "Bearer " <> gptApiKey backend)
              , ("Content-Type", "application/json")
              ]
          , httpJsonBody = Just (gptRequestJson request)
          }
    pure $ do
      response <- result
      if httpStatus response >= 200 && httpStatus response < 300
        then
          Right (gptResponseFromJson (httpJson response))
        else
          Left
            (LlmHttpError (httpStatus response) "OpenAI response returned non-success status")

gptInputItem :: LlmInputItem -> Value
gptInputItem (InputMessage message) =
  object
    [ "role" .= roleText (messageRole message)
    , "content" .= fmap gptContentPart (messageContent message)
    ]
gptInputItem (InputToolOutput output) =
  object
    [ "type" .= String "function_call_output"
    , "call_id" .= unToolCallId (toolOutputCallId output)
    , "output" .= toolOutputText output
    ]
gptInputItem (InputResponse (ResponseMessage message)) =
  object
    [ "role" .= roleText (messageRole message)
    , "content" .= map outputPart (messageContent message)
    ]
 where
  outputPart (TextPart text) = object ["type" .= String "output_text", "text" .= text]
  outputPart part = gptContentPart part
gptInputItem (InputResponse (ResponseToolCall call)) =
  object
    [ "type" .= String "function_call"
    , "call_id" .= unToolCallId (toolCallId call)
    , "name" .= unToolName (toolCallName call)
    , "arguments" .= jsonText (toolCallArguments call)
    ]
gptInputItem (InputResponse (ResponseToolOutput output)) = gptInputItem (InputToolOutput output)
gptInputItem (InputResponse (ResponseOther raw)) = raw
gptInputItem (InputResponse ResponseReasoning{reasoningSummary, reasoningEncryptedContent}) =
  object
    ( [ "type" .= String "reasoning"
      , "summary"
          .= map
            (\s -> object ["type" .= String "summary_text", "text" .= s])
            reasoningSummary
      ]
        <> [ "encrypted_content" .= encrypted | Just encrypted <- [reasoningEncryptedContent]
           ]
    )

gptContentPart :: ContentPart -> Value
gptContentPart (TextPart text) =
  object ["type" .= String "input_text", "text" .= text]
gptContentPart (LinkPart url) =
  object ["type" .= String "input_text", "text" .= url]
gptContentPart (ImagePart media detail) =
  object
    [ "type" .= String "input_image"
    , "image_url" .= mediaRefText media
    , "detail" .= fmap imageDetailText detail
    ]
gptContentPart (AudioPart media) =
  object ["type" .= String "input_audio", "audio_url" .= mediaRefText media]
gptContentPart (FilePart media) =
  object ["type" .= String "input_file", "file_url" .= mediaRefText media]
gptContentPart (JsonPart value) =
  object ["type" .= String "input_text", "text" .= jsonText value]
gptContentPart (ToolCallPart call) =
  object
    [ "type" .= String "function_call"
    , "call_id" .= unToolCallId (toolCallId call)
    , "name" .= unToolName (toolCallName call)
    , "arguments" .= toolCallArguments call
    ]
gptContentPart (ToolOutputPart output) = gptInputItem (InputToolOutput output)
gptContentPart (RefusalPart text) =
  object ["type" .= String "refusal", "refusal" .= text]

gptToolSpec :: ToolSpec -> Maybe Value
gptToolSpec FunctionTool{toolSpecName, toolSpecDescription, toolSpecParameters} =
  Just
    ( object
        [ "type" .= String "function"
        , "name" .= unToolName toolSpecName
        , "description" .= toolSpecDescription
        , "parameters" .= toolSpecParameters
        ]
    )
gptToolSpec WebSearchTool{} =
  Just (object ["type" .= String "web_search"])
gptToolSpec FreeformTool{toolSpecName, toolSpecDescription} =
  Just
    ( object
        [ "type" .= String "custom"
        , "name" .= unToolName toolSpecName
        , "description" .= toolSpecDescription
        ]
    )
gptToolSpec NamespaceTool{} = Nothing

gptToolChoice :: ToolChoice -> Value
gptToolChoice ToolAuto = String "auto"
gptToolChoice ToolNone = String "none"
gptToolChoice ToolRequired = String "required"
gptToolChoice (ToolNamed name) =
  object ["type" .= String "function", "name" .= unToolName name]

parseOutput :: Value -> Parser [LlmResponseItem]
parseOutput =
  withObject "GPT response" $ \o ->
    o .: "output"
      >>= withArray "output" (pure . catMaybes . fmap parseOutputItem . toList)

parseOutputItem :: Value -> Maybe LlmResponseItem
parseOutputItem value =
  parseMaybe parseFunctionCall value
    <|> parseMaybe parseMessage value
    <|> Just (ResponseOther value)

parseFunctionCall :: Value -> Parser LlmResponseItem
parseFunctionCall =
  withObject "function_call" $ \o -> do
    itemType <- o .: "type"
    if itemType == String "function_call"
      then do
        name <- o .: "name"
        callId <- o .: "call_id"
        argumentsText <- o .:? "arguments"
        let arguments =
              maybe (Object mempty) id (argumentsText >>= decodeStrict' . Text.encodeUtf8)
        pure
          ( ResponseToolCall
              ToolCall
                { toolCallId = ToolCallId callId
                , toolCallName = ToolName name
                , toolCallNamespace = Nothing
                , toolCallArguments = arguments
                , toolCallStatus = Just ToolCallCompleted
                }
          )
      else fail "not a function_call"

parseMessage :: Value -> Parser LlmResponseItem
parseMessage =
  withObject "message" $ \o -> do
    itemType <- o .: "type"
    if itemType == String "message"
      then do
        content <- o .: "content"
        text <-
          withArray
            "content"
            (pure . Text.intercalate "\n" . catMaybes . fmap outputText . toList)
            content
        pure
          ( ResponseMessage
              LlmMessage
                { messageId = Nothing
                , messageRole = AssistantRole
                , messageContent = [TextPart text]
                , messagePhase = Nothing
                , messageMetadata = mempty
                }
          )
      else fail "not a message"

outputText :: Value -> Maybe Text
outputText =
  parseMaybe $
    withObject "output_text" $ \o -> do
      itemType <- o .: "type"
      if itemType == String "output_text"
        then o .: "text"
        else fail "not output_text"

roleText :: Role -> Text
roleText SystemRole = "system"
roleText DeveloperRole = "developer"
roleText UserRole = "user"
roleText AssistantRole = "assistant"
roleText ToolRole = "tool"
roleText (AgentRole name) = name

imageDetailText :: ImageDetail -> Text
imageDetailText ImageAuto = "auto"
imageDetailText ImageLow = "low"
imageDetailText ImageHigh = "high"
imageDetailText ImageOriginal = "original"

mediaRefText :: MediaRef -> Text
mediaRefText (MediaUrl url) = url
mediaRefText (MediaFile path) = Text.pack path
mediaRefText (MediaDataUrl url) = url
mediaRefText MediaBytes{mediaMimeType, mediaBytesBase64} =
  "data:" <> mediaMimeType <> ";base64," <> mediaBytesBase64

toolOutputText :: ToolOutput -> Text
toolOutputText output =
  Text.intercalate "\n" (toolOutputContentText <$> toolOutputContent output)

toolOutputContentText :: ToolOutputContent -> Text
toolOutputContentText (ToolOutputText text) = text
toolOutputContentText (ToolOutputImage url) = url
toolOutputContentText (ToolOutputAudio url) = url
toolOutputContentText (ToolOutputJson value) = jsonText value
toolOutputContentText (ToolOutputEncrypted text) = text

mapMaybeTool :: (a -> Maybe b) -> [a] -> [b]
mapMaybeTool f = catMaybes . fmap f

fromMaybeOther :: Value -> Maybe [LlmResponseItem] -> [LlmResponseItem]
fromMaybeOther raw Nothing = [ResponseOther raw]
fromMaybeOther _ (Just items) = items

jsonText :: Value -> Text
jsonText = Text.decodeUtf8 . BL.toStrict . encode
