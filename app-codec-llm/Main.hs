module Main (main) where

import Autodocodec
import Autodocodec.Yaml
import Data.Aeson (decodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as LBL
import Data.Ollama.Chat
import DocumentTypes.CV
import Options.Applicative qualified as Opt
import Path.Posix
import PromptResolution
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)

data CLIArgs = CLIArgs
  { contextInputFile :: FilePath,
    cvInputFile :: FilePath,
    documentOldInputFile :: Maybe FilePath,
    documentOutputFile :: Maybe FilePath
  }
  deriving (Show)

cliArgs :: Opt.Parser CLIArgs
cliArgs =
  CLIArgs
    <$> Opt.strOption
      ( Opt.long "context-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input .json file defining the LLM context and prompts"
      )
    <*> Opt.strOption
      ( Opt.long "cv-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input .json file defining the CV contents"
      )
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.long "in-file"
              <> Opt.metavar "FILE"
              <> Opt.help "The old CV version .json document (if any), to enable caching."
          )
      )
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.long "out-file"
              <> Opt.metavar "FILE"
              <> Opt.help "Output .json document if all is succesful. If absent the output is sent to stdout."
          )
      )

ollamaConfig :: OllamaConfig
ollamaConfig =
  OllamaConfig
    { hostUrl = "http://127.0.0.1:11434", -- TODO: make it passable as CLI argument
      timeout = 180,
      onModelStart = Nothing,
      onModelFinish = Nothing,
      onModelError = Nothing,
      retryCount = Nothing,
      retryDelay = Nothing,
      commonManager = Nothing -- TODO: add common manager
    }

main :: IO ()
main = do
  CLIArgs {..} <- Opt.execParser cliOpts
  contextPosixPath <- parseRelFile contextInputFile
  cvPosixPath <- parseRelFile cvInputFile
  readYamlConfigFile contextPosixPath >>= \case
    Nothing -> pure ()
    Just (context :: ContextCV) -> do
      readYamlConfigFile cvPosixPath >>= \case
        Nothing -> pure ()
        Just (cv :: CV TextAtom) -> do
          case documentOldInputFile of
            Nothing -> resolveLLM documentOutputFile context cv Nothing
            Just oldFile -> do
              exists <- doesFileExist oldFile
              if exists
                then decodeFileStrict oldFile >>= resolveLLM documentOutputFile context cv
                else do
                  hPutStrLn stderr $ unwords ["Old CV file does not exist!", oldFile]
                  resolveLLM documentOutputFile context cv Nothing
  where
    cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
    resolveLLM documentOutputFileMaybe context cv oldCVMaybe = do
      result <- resolvePrompts ollamaConfig $ preparePrompts context cv oldCVMaybe
      case documentOutputFileMaybe of
        Nothing -> LBL.putStrLn $ encodePretty (toJSONViaCodec result)
        Just outFile -> LBL.writeFile outFile $ encodePretty (toJSONViaCodec result)
