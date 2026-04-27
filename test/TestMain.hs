module TestMain (main) where

import ArbitraryCV ()
import ArbitraryContextCV ()
import ArbitraryCoverLetter ()
import Autodocodec
import Data.Aeson (FromJSON, Result (..), fromJSON)
import Data.Foldable (traverse_)
import Data.Text
import DocumentTypes.CV
import DocumentTypes.Common
import DocumentTypes.CoverLetter
import PromptResolution (preparePrompts)
import Samples
  ( cacheC,
    documentT,
    documentT1,
    documentT2,
    resultR1,
    resultR2,
    testContextCV,
  )
import ShallowCV ()
import ShallowEq (shouldBeShallow)
import Test.Hspec
import Test.Hspec.QuickCheck
import Prelude

spec :: Spec
spec = modifyMaxSize (const 20) $ do
  describe "Cache tests" $ do
    describe "Given any document and no old document" $
      describe "When we call preparePrompts" $
        prop "Then all results of preparePrompts call the prompt (no caching results)" $
          \document -> do
            let cv = preparePrompts testContextCV document Nothing
            traverse_ (\p -> isRunOllamaPrompt p `shouldBe` True) cv
    describe "Given any document and an identical old document" $
      describe "When we call preparePrompts" $
        prop "Then all results of preparePrompts are cached (no prompt is called)" $
          \document -> do
            let inputDocument = preparePrompts testContextCV document Nothing
                cacheDocument = fmap mkCache inputDocument
                newDocument = preparePrompts testContextCV document (Just cacheDocument)
            traverse_ (\p -> isKeepCachedResult p `shouldBe` True) newDocument
    describe "Given test document T and cached document C" $
      it "Then cached document C matches the result of resolving T" $ do
        let expected = preparePrompts testContextCV documentT Nothing
        cacheC `shouldBe` fmap mkCache expected
    describe "Given test document T1 (T with a changed section) and cached document C" $
      describe "When we call preparePrompts" $
        it "Then the changed section triggers a prompt and the other fields are cached" $ do
          let newDocument = preparePrompts testContextCV documentT1 (Just cacheC)
          newDocument `shouldBeShallow` resultR1
    describe "Given test document T2 (T with an added Practice) and cached document C" $
      describe "When we call preparePrompts" $
        it "Then the added practice triggers a prompt and the other fields are cached" $ do
          let newDocument = preparePrompts testContextCV documentT2 (Just cacheC)
          newDocument `shouldBeShallow` resultR2
  describe "JSON tests" $ do
    prop "CV roundtrip" $ propCodecRoundtrip @(CV TextAtom)
    prop "CV prompt result roundtrip" $ propCodecRoundtrip @(CV LLMPromptResultText)
    prop "Cover letter rountrip" $ propCodecRoundtrip @(CoverLetter Text)
    prop "ContextCV roundtrip" $ propCodecRoundtrip @ContextCV

propCodecRoundtrip :: (FromJSON a, Eq a, HasCodec a) => a -> Bool
propCodecRoundtrip val =
  let json = toJSONViaCodec val
   in case fromJSON json of
        Success decoded -> decoded == val
        Error _ -> False

mkCache :: LLMPromptParameters -> LLMPromptResultText
mkCache (RunOllamaPrompt _chatOps inputHashMD5) =
  LLMPromptResultText {getPromptResultText = "TEST SUITE", ..}
mkCache (KeepCachedResult getPromptResultText inputHashMD5) = LLMPromptResultText {..}

isRunOllamaPrompt :: LLMPromptParameters -> Bool
isRunOllamaPrompt RunOllamaPrompt {} = True
isRunOllamaPrompt _ = False

isKeepCachedResult :: LLMPromptParameters -> Bool
isKeepCachedResult KeepCachedResult {} = True
isKeepCachedResult _ = False

main :: IO ()
main = hspec spec
