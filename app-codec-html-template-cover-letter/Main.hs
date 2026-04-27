module Main (main) where

import Control.Monad
import Data.Aeson (Value (Object), eitherDecodeFileStrict, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text
import Data.Text.Lazy.IO qualified as TIO
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import DocumentTypes.CoverLetter (CoverLetter)
import Options.Applicative qualified as Opt
import System.IO (hPutStrLn, stderr)
import Text.Mustache

data CLIArgs = CLIArgs
  { letterInputFile :: FilePath,
    letterTemplateFile :: FilePath,
    templateLanguage :: Maybe String
  }
  deriving (Show)

cliArgs :: Opt.Parser CLIArgs
cliArgs =
  CLIArgs
    <$> Opt.strOption
      ( Opt.long "cover-letter-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input cover letter file (JSON/YAML)"
      )
    <*> Opt.strOption
      ( Opt.long "template-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input .mustache template file"
      )
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.long "language"
              <> Opt.help "Language parameter, relayed to the mustache template. Default 'english'"
          )
      )

main :: IO ()
main = do
  CLIArgs {..} <- Opt.execParser cliOpts
  eitherDecodeFileStrict letterInputFile >>= \case
    Left err -> hPutStrLn stderr err
    Right (letter :: CoverLetter Text) -> do
      template <- compileMustacheFile letterTemplateFile
      today <- utctDay <$> getCurrentTime
      let simpleCV = fmap removeTrailingPunctuation letter
          (warnings, output) =
            renderMustacheW template $
              mergeObjects (toJSON simpleCV) (extraFields today templateLanguage)
      TIO.putStrLn output
      unless (Prelude.null warnings) $ do
        hPutStrLn stderr "Mustache warnings:"
        forM_ warnings $ hPutStrLn stderr . displayMustacheWarning
  where
    removeTrailingPunctuation =
      dropWhileEnd
        (\c -> c `Prelude.elem` ['.', ',', ';', ':', '\r', '\n'])
    extraFields today maybeLanguage =
      object
        [ "today" .= iso8601Show today,
          fromString (fromMaybe "english" maybeLanguage) .= True
        ]
    cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object (KM.union a b)
mergeObjects _ _ = error "logical error, expecting just objects"
