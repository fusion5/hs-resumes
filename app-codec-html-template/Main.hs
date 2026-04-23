module Main (main) where

import Control.Monad
import Data.Aeson (Value (Object), eitherDecodeFileStrict, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Text
import Data.Text.Lazy.IO qualified as TIO
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import DocumentTypes
import Options.Applicative qualified as Opt
import System.IO (hPutStrLn, stderr)
import Text.Mustache

data CLIArgs = CLIArgs
  { cvInputFile :: FilePath,
    cvTemplateFile :: FilePath
  }
  deriving (Show)

cliArgs :: Opt.Parser CLIArgs
cliArgs =
  CLIArgs
    <$> Opt.strOption
      ( Opt.long "cv-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input CV file as output by 'app-codec-llm'"
      )
    <*> Opt.strOption
      ( Opt.long "template-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input .mustache template file"
      )

main :: IO ()
main = do
  CLIArgs {..} <- Opt.execParser cliOpts
  eitherDecodeFileStrict cvInputFile >>= \case
    Left err -> hPutStrLn stderr err
    Right (cv :: CV LLMPromptResultText) -> do
      template <- compileMustacheFile cvTemplateFile
      today <- utctDay <$> getCurrentTime
      let simpleCV = fmap (removeTrailingPunctuation . getPromptResultText) cv
          (warnings, output) =
            renderMustacheW template $ mergeObjects (toJSON simpleCV) (extraFields today)
      TIO.putStrLn output
      unless (Prelude.null warnings) $ do
        hPutStrLn stderr "Mustache warnings:"
        forM_ warnings $ hPutStrLn stderr . displayMustacheWarning
  where
    removeTrailingPunctuation =
      dropWhileEnd
        (\c -> c `Prelude.elem` ['.', ',', ';', ':', '\r', '\n'])
    extraFields today = object ["today" .= iso8601Show today]
    cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object (KM.union a b)
mergeObjects _ _ = error "logical error, expecting just objects"
