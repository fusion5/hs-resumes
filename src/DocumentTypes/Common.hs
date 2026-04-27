module DocumentTypes.Common (module DocumentTypes.Common) where

import Data.ByteString
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BC
import Data.Ollama.Chat
import Data.String (IsString (..))
import Data.Text

-- Represents the original, input body text to be transformed by the LLM
newtype TextAtom = TextAtom {getText :: Text}
  deriving (Show, Eq, IsString)

newtype MD5Hash = MD5Hash {getHash :: ByteString}
  deriving (Eq)

instance Show MD5Hash where
  show (MD5Hash bs) = BC.unpack (B16.encode bs)

instance IsString MD5Hash where
  fromString s = case B16.decode (BC.pack s) of
    Right bs -> MD5Hash bs
    Left err -> error $ "Invalid hex string literal: " ++ err

data LLMPromptResultText = LLMPromptResultText
  { getPromptResultText :: Text,
    -- | The hash of the LLM configuration and input messages that produced this LLM text
    inputHashMD5 :: MD5Hash
  }
  deriving (Show, Eq)

data LLMPromptParameters
  = -- | Consider abstracting this away from Ollama? Not needed for now
    RunOllamaPrompt ChatOps MD5Hash
  | KeepCachedResult Text MD5Hash
  deriving (Show, Eq)
