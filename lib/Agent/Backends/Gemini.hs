module Agent.Backends.Gemini
  ( Gemini (..)
  , defaultGeminiBaseUrl
  , geminiRequestJson
  , geminiResponseFromJson
  )
where

import Agent.PickyFork.Llm
import Agent.PickyFork.Llm.Transport
import Control.Applicative ((<|>))
import Data.Aeson
  ( Value (..)
  , encode
  , object
  , withArray
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

data Gemini transport = Gemini
  { geminiApiKey :: Text
  , geminiBaseUrl :: Text
  , geminiTransport :: transport
  }

defaultGeminiBaseUrl :: Text
defaultGeminiBaseUrl = "https://generativelanguage.googleapis.com/v1beta/models"

geminiRequestJson :: LlmRequest -> Value
geminiRequestJson request =
  object
    [ "contents" .= fmap geminiInputItem (requestInput request)
    , "tools" .= geminiTools (requestTools request)
    , "toolConfig" .= geminiToolConfig (requestToolChoice request)
    , "generationConfig" .= geminiGenerationConfig (requestParams request)
    ]

geminiResponseFromJson :: Value -> LlmResponse
geminiResponseFromJson raw =
  LlmResponse
    { responseItems = fromMaybeOther raw (parseMaybe parseCandidates raw)
    }

instance LlmTransport transport => Llm (Gemini transport) where
  llmName _ = "gemini"

  runLlm backend request = do
    result <-
      sendJsonRequest
        (geminiTransport backend)
        HttpRequest
          { httpMethod = HttpPost
          , httpUrl =
              geminiBaseUrl backend
                <> "/"
                <> unModelName (requestModel request)
                <> ":generateContent"
          , httpHeaders =
              [ ("x-goog-api-key", geminiApiKey backend)
              , ("Content-Type", "application/json")
              ]
          , httpJsonBody = Just (geminiRequestJson request)
          }
    pure $ do
      response <- result
      if httpStatus response >= 200 && httpStatus response < 300
        then
          Right (geminiResponseFromJson (httpJson response))
        else
          Left
            (LlmHttpError (httpStatus response) "Gemini response returned non-success status")

geminiInputItem :: LlmInputItem -> Value
geminiInputItem (InputMessage message) =
  object
    [ "role" .= geminiRole (messageRole message)
    , "parts" .= fmap geminiContentPart (messageContent message)
    ]
geminiInputItem (InputToolOutput output) =
  object
    [ "role" .= String "user"
    , "parts" .= [geminiToolOutput output]
    ]
geminiInputItem (InputResponse (ResponseMessage message)) = geminiInputItem (InputMessage message)
geminiInputItem (InputResponse (ResponseToolCall call)) =
  object
    ["role" .= String "model", "parts" .= [geminiContentPart (ToolCallPart call)]]
geminiInputItem (InputResponse (ResponseToolOutput output)) = geminiInputItem (InputToolOutput output)
geminiInputItem (InputResponse (ResponseOther raw)) =
  object ["role" .= String "model", "parts" .= [raw]]
geminiInputItem (InputResponse ResponseReasoning{reasoningSummary}) =
  object
    [ "role" .= String "model"
    , "parts" .= map (\s -> object ["text" .= s, "thought" .= True]) reasoningSummary
    ]

geminiContentPart :: ContentPart -> Value
geminiContentPart (TextPart text) = object ["text" .= text]
geminiContentPart (LinkPart url) = object ["text" .= url]
geminiContentPart (ImagePart media _) = geminiFilePart media
geminiContentPart (AudioPart media) = geminiFilePart media
geminiContentPart (FilePart media) = geminiFilePart media
geminiContentPart (JsonPart value) = object ["text" .= Text.decodeUtf8 (BL.toStrict (encode value))]
geminiContentPart (ToolCallPart call) =
  object
    [ "functionCall"
        .= object
          ["name" .= unToolName (toolCallName call), "args" .= toolCallArguments call]
    ]
geminiContentPart (ToolOutputPart output) = geminiToolOutput output
geminiContentPart (RefusalPart text) = object ["text" .= text]

geminiToolOutput :: ToolOutput -> Value
geminiToolOutput output =
  object
    [ "functionResponse"
        .= object
          [ "id" .= unToolCallId (toolOutputCallId output)
          , "name" .= fmap unToolName (toolOutputName output)
          , "response" .= toolOutputJson output
          ]
    ]

geminiTools :: [ToolSpec] -> [Value]
geminiTools tools =
  [ object
      ["functionDeclarations" .= catMaybes (geminiFunctionDeclaration <$> tools)]
  | any isFunctionTool tools
  ]

geminiFunctionDeclaration :: ToolSpec -> Maybe Value
geminiFunctionDeclaration FunctionTool{toolSpecName, toolSpecDescription, toolSpecParameters} =
  Just
    ( object
        [ "name" .= unToolName toolSpecName
        , "description" .= toolSpecDescription
        , "parameters" .= toolSpecParameters
        ]
    )
geminiFunctionDeclaration _ = Nothing

geminiToolConfig :: ToolChoice -> Value
geminiToolConfig choice =
  object
    [ "functionCallingConfig"
        .= object
          ( [ "mode" .= geminiToolMode choice
            ]
              <> namedChoice choice
          )
    ]

geminiGenerationConfig :: GenerationParams -> Value
geminiGenerationConfig params =
  object
    [ "temperature" .= generationTemperature params
    , "topP" .= generationTopP params
    , "maxOutputTokens" .= generationMaxOutputTokens params
    , "stopSequences" .= generationStop params
    ]

parseCandidates :: Value -> Parser [LlmResponseItem]
parseCandidates =
  withObject "Gemini response" $ \o -> do
    candidates <- o .: "candidates"
    withArray
      "candidates"
      (fmap concat . traverse parseCandidate . toList)
      candidates

parseCandidate :: Value -> Parser [LlmResponseItem]
parseCandidate =
  withObject "candidate" $ \o -> do
    content <- o .: "content"
    withObject
      "content"
      ( \co ->
          co .: "parts" >>= withArray "parts" (pure . catMaybes . fmap parsePart . toList)
      )
      content

parsePart :: Value -> Maybe LlmResponseItem
parsePart value =
  parseMaybe parseFunctionCall value
    <|> parseMaybe parseText value
    <|> Just (ResponseOther value)

parseFunctionCall :: Value -> Parser LlmResponseItem
parseFunctionCall =
  withObject "part" $ \o -> do
    call <- o .: "functionCall"
    withObject
      "functionCall"
      ( \co -> do
          name <- co .: "name"
          args <- co .:? "args"
          callId <- co .:? "id"
          pure
            ( ResponseToolCall
                ToolCall
                  { toolCallId = ToolCallId (maybe name id callId)
                  , toolCallName = ToolName name
                  , toolCallNamespace = Nothing
                  , toolCallArguments = maybe (Object mempty) id args
                  , toolCallStatus = Just ToolCallCompleted
                  }
            )
      )
      call

parseText :: Value -> Parser LlmResponseItem
parseText =
  withObject "part" $ \o -> do
    text <- o .: "text"
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

geminiRole :: Role -> Text
geminiRole UserRole = "user"
geminiRole ToolRole = "user"
geminiRole AssistantRole = "model"
geminiRole _ = "user"

geminiFilePart :: MediaRef -> Value
geminiFilePart (MediaUrl url) = object ["fileData" .= object ["fileUri" .= url]]
geminiFilePart (MediaFile path) = object ["fileData" .= object ["fileUri" .= Text.pack path]]
geminiFilePart (MediaDataUrl url) = object ["fileData" .= object ["fileUri" .= url]]
geminiFilePart MediaBytes{mediaMimeType, mediaBytesBase64} =
  object
    [ "inlineData" .= object ["mimeType" .= mediaMimeType, "data" .= mediaBytesBase64]
    ]

geminiToolMode :: ToolChoice -> Text
geminiToolMode ToolAuto = "AUTO"
geminiToolMode ToolNone = "NONE"
geminiToolMode ToolRequired = "ANY"
geminiToolMode (ToolNamed _) = "ANY"

namedChoice :: ToolChoice -> [Pair]
namedChoice (ToolNamed name) = ["allowedFunctionNames" .= [unToolName name]]
namedChoice _ = []

isFunctionTool :: ToolSpec -> Bool
isFunctionTool FunctionTool{} = True
isFunctionTool _ = False

toolOutputJson :: ToolOutput -> Value
toolOutputJson output =
  object
    [ "content" .= fmap toolOutputContentJson (toolOutputContent output)
    , "success" .= toolOutputSuccess output
    ]

toolOutputContentJson :: ToolOutputContent -> Value
toolOutputContentJson (ToolOutputText text) = object ["text" .= text]
toolOutputContentJson (ToolOutputImage url) = object ["image_url" .= url]
toolOutputContentJson (ToolOutputAudio url) = object ["audio_url" .= url]
toolOutputContentJson (ToolOutputJson value) = value
toolOutputContentJson (ToolOutputEncrypted text) = object ["encrypted_content" .= text]

fromMaybeOther :: Value -> Maybe [LlmResponseItem] -> [LlmResponseItem]
fromMaybeOther raw Nothing = [ResponseOther raw]
fromMaybeOther _ (Just items) = items
