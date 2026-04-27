{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module PromptResolution
  ( preparePrompts,
    resolvePrompts,
  )
where

import Codec.Serialise
import Control.Monad (zipWithM)
import Control.Monad.Reader
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString.Char8 qualified as BS
import Data.List.NonEmpty as NE
import Data.Ollama.Chat
import Data.Sequence as Seq
import Data.Text as Text
import DocumentTypes.CV
import GHC.Generics
import GHC.IsList qualified as IsList
import Text.Pretty.Simple (pPrint)
import Prelude

-- | Maintains prompt context (kind of like a stack/environment)
type TraverseAction a = Reader (Seq Text) a

preparePrompts ::
  ContextCV -> CV TextAtom -> Maybe (CV LLMPromptResultText) -> CV LLMPromptParameters
preparePrompts context maybeOldCV cv = runReader (prepare context maybeOldCV cv) mempty

-- | Runs the actual LLM commands
resolvePrompts :: OllamaConfig -> CV LLMPromptParameters -> IO (CV LLMPromptResultText)
resolvePrompts ollamaConfig = traverse (resolveAtom ollamaConfig)

resolveAtom :: OllamaConfig -> LLMPromptParameters -> IO LLMPromptResultText
resolveAtom _ (KeepCachedResult getPromptResultText inputHashMD5) = pure $ LLMPromptResultText {..}
resolveAtom ollamaConfig (RunOllamaPrompt chatOps messagesHash) = do
  putStr ">>> "
  pPrint chatOps
  if runLLM
    then
      chat chatOps (Just ollamaConfig) >>= \case
        Left ollamaError -> do
          print ollamaError
          pure $ LLMPromptResultText "" messagesHash
        Right (ChatResponse {message = Just (Message {..})}) -> do
          putStr "<<< "
          pPrint content
          pure $ LLMPromptResultText content messagesHash
        Right (ChatResponse {message = Nothing}) -> do
          -- print ("Error! Empty response!" :: String)
          pure $ LLMPromptResultText "" messagesHash
    else
      pure $ LLMPromptResultText "" messagesHash
  where
    runLLM = True

deriving instance Generic Role

deriving instance Generic ModelOptions

deriving instance Serialise Role

deriving instance Serialise ModelOptions

instance Serialise Message where
  encode (Message {..}) = encode (role, content, images, thinking)
  decode = error "Unsupported"

instance Serialise ChatOps where
  encode (ChatOps {..}) = encode (modelName, messages, keepAlive, options, think)
  decode = error "Unsupported"

prepareAtom ::
  Text ->
  LLMInstructions ->
  TextAtom ->
  Maybe LLMPromptResultText ->
  TraverseAction LLMPromptParameters
prepareAtom modelName LLMInstructions {..} promptBody oldTextMaybe = local (|> llmContext) $ do
  contextStack <- ask
  let messages =
        fmap mkSystemMessage contextStack
          |> mkSystemMessage llmImperative
          |> mkUserMessage (getText promptBody)
      cfg = chatConfig modelName $ NE.fromList $ IsList.toList messages
      messagesHash = MD5Hash $ MD5.hash $ BS.toStrict $ serialise cfg
  case oldTextMaybe of
    Nothing -> pure $ RunOllamaPrompt cfg messagesHash
    Just LLMPromptResultText {..} ->
      if messagesHash == inputHashMD5
        then pure $ KeepCachedResult getPromptResultText inputHashMD5
        else pure $ RunOllamaPrompt cfg messagesHash

prepare ::
  ContextCV ->
  CV TextAtom ->
  Maybe (CV LLMPromptResultText) ->
  TraverseAction (CV LLMPromptParameters)
prepare ContextCV {..} new oldMay = do
  local (|> llmContext ctxCVContext) $ do
    newLocation <-
      prepareAtom
        ctxModelName
        ctxCVLocation
        (currentLocation new)
        (currentLocation <$> oldMay)
    newSummary <-
      prepareSection
        ctxModelName
        ctxSummary
        (cvSummary new)
        (cvSummary <$> oldMay)
        (prepareAtom ctxModelName)
    newObjective <-
      prepareSection
        ctxModelName
        ctxObjective
        (cvObjective new)
        (cvObjective <$> oldMay)
        (prepareAtom ctxModelName)
    newProgrammingLanguages <-
      prepareSection
        ctxModelName
        ctxProgrammingLanguage
        (cvProgrammingLanguages new)
        (cvProgrammingLanguages <$> oldMay)
        (\_ p _ -> pure p)
    newTechnologies <-
      prepareSection
        ctxModelName
        ctxTechnologies
        (cvTechnologies new)
        (cvTechnologies <$> oldMay)
        (\_ p _ -> pure p)
    newStandards <-
      prepareSection
        ctxModelName
        ctxStandards
        (cvStandards new)
        (cvStandards <$> oldMay)
        (\_ p _ -> pure p)
    newPractices <-
      prepareSection
        ctxModelName
        ctxPractices
        (cvPractices new)
        (cvPractices <$> oldMay)
        (zipWithMaybeM . preparePractice ctxModelName)
    newLanguages <-
      prepareSection
        ctxModelName
        ctxSpokenLanguages
        (cvSpokenLanguages new)
        (cvSpokenLanguages <$> oldMay)
        (zipWithMaybeM . prepareSpokenLanguage ctxModelName)
    newWorkExperience <-
      prepareSection
        ctxModelName
        ctxWorkExperience
        (cvWorkExperience new)
        (cvWorkExperience <$> oldMay)
        (zipWithMaybeM . prepareWorkExperience ctxModelName)
    newEducation <-
      prepareSection
        ctxModelName
        ctxEducation
        (cvEducation new)
        (cvEducation <$> oldMay)
        (zipWithMaybeM . prepareEducation ctxModelName)
    newAwards <-
      prepareSection
        ctxModelName
        ctxAwards
        (cvAwards new)
        (cvAwards <$> oldMay)
        (zipWithMaybeM . prepareAwards ctxModelName)
    pure
      CV
        { currentLocation = newLocation,
          cvSummary = newSummary,
          cvObjective = newObjective,
          cvProgrammingLanguages = newProgrammingLanguages,
          cvTechnologies = newTechnologies,
          cvStandards = newStandards,
          cvPractices = newPractices,
          cvSpokenLanguages = newLanguages,
          cvWorkExperience = newWorkExperience,
          cvEducation = newEducation,
          cvAwards = newAwards
        }

prepareWorkExperience ::
  Text ->
  ContextWorkExperience ->
  WorkExperience TextAtom ->
  Maybe (WorkExperience LLMPromptResultText) ->
  TraverseAction (WorkExperience LLMPromptParameters)
prepareWorkExperience modelName ContextWorkExperience {..} new oldMay =
  WorkExperience (startTime new) (endTime new) (position new) (employer new)
    <$> prepareAtom modelName ctxweLocation (workLocation new) (workLocation <$> oldMay)
    <*> prepareExperienceType modelName ctxweJob (job new) (job <$> oldMay)

-- | Only if the old constructor matches the new do we pass on the previous value, otherwise we
-- pass Nothing.
prepareExperienceType ::
  Text ->
  ContextExperienceType ->
  ExperienceType TextAtom ->
  Maybe (ExperienceType LLMPromptResultText) ->
  TraverseAction (ExperienceType LLMPromptParameters)
prepareExperienceType
  modelName
  (ContextExperienceType {..})
  (EmployeeExperience newEmployee)
  (Just (EmployeeExperience oldEmployee)) =
    EmployeeExperience <$> prepareEmployee modelName ctxEmployee newEmployee (Just oldEmployee)
prepareExperienceType
  modelName
  (ContextExperienceType {..})
  (EmployeeExperience newEmployee)
  _ =
    -- the new constructor is different. there's no way to use the cache from the old one.
    EmployeeExperience <$> prepareEmployee modelName ctxEmployee newEmployee Nothing
prepareExperienceType
  modelName
  (ContextExperienceType {..})
  (SelfEmployedExperience newSelfEmployed)
  (Just (SelfEmployedExperience oldSelfEmployed)) =
    SelfEmployedExperience
      <$> prepareSelfEmployed modelName ctxSelfEmployed newSelfEmployed (Just oldSelfEmployed)
prepareExperienceType
  modelName
  (ContextExperienceType {..})
  (SelfEmployedExperience newSelfEmployed)
  _ =
    SelfEmployedExperience <$> prepareSelfEmployed modelName ctxSelfEmployed newSelfEmployed Nothing

prepareEducation ::
  Text ->
  ContextEducation ->
  Education TextAtom ->
  Maybe (Education LLMPromptResultText) ->
  TraverseAction (Education LLMPromptParameters)
prepareEducation modelName ContextEducation {..} new oldMay =
  Education (eduStartTime new) (eduEndTime new)
    <$> prepareAtom modelName ctxeduDegree (eduDegree new) (eduDegree <$> oldMay)
    <*> pure (eduInstitution new)
    <*> prepareAtom modelName ctxeduLocation (eduLocation new) (eduLocation <$> oldMay)
    <*> zipWithMaybeM
      (prepareEducationDescription modelName ctxeduDescription)
      (eduDescriptions new)
      (eduDescriptions <$> oldMay)

prepareAwards ::
  Text ->
  ContextAward ->
  Awards TextAtom ->
  Maybe (Awards LLMPromptResultText) ->
  TraverseAction (Awards LLMPromptParameters)
prepareAwards modelName ContextAward {..} new oldMay =
  Awards (awardTime new)
    <$> prepareAtom modelName ctxAward (awardDescription new) (awardDescription <$> oldMay)

prepareEducationDescription ::
  Text ->
  ContextEducationDescription ->
  EducationDescription TextAtom ->
  Maybe (EducationDescription LLMPromptResultText) ->
  TraverseAction (EducationDescription LLMPromptParameters)
prepareEducationDescription modelName ContextEducationDescription {..} new oldMay =
  EducationDescription
    <$> prepareAtom modelName ctxeddDescription (eduDescription new) (eduDescription <$> oldMay)
    <*> pure (eduLanguages new)
    <*> pure (eduTechnologies new)
    <*> pure (eduStandards new)
    <*> zipWithMaybeM
      (preparePractice modelName ctxeddSWEPractices)
      (eduPractices new)
      (eduPractices <$> oldMay)

prepareEmployee ::
  Text ->
  ContextEmployee ->
  Employee TextAtom ->
  Maybe (Employee LLMPromptResultText) ->
  TraverseAction (Employee LLMPromptParameters)
prepareEmployee modelName ContextEmployee {..} new oldMaybe = do
  Employee
    <$> prepareAtom
      modelName
      ctxeEmployerDescription
      (employerDescription new)
      (employerDescription <$> oldMaybe)
    <*> zipWithMaybeM
      (prepareResponsibility modelName ctxeResponsibilities)
      (employerResponsibilities new)
      (employerResponsibilities <$> oldMaybe)

prepareResponsibility ::
  Text ->
  ContextResponsibility ->
  Responsibility TextAtom ->
  Maybe (Responsibility LLMPromptResultText) ->
  TraverseAction (Responsibility LLMPromptParameters)
prepareResponsibility modelName ContextResponsibility {..} newResponsibility oldResponsibility =
  Responsibility
    <$> prepareAtom
      modelName
      ctxrespDescription
      (description newResponsibility)
      (description <$> oldResponsibility)
    <*> pure (languages newResponsibility)
    <*> pure (technologies newResponsibility)
    <*> pure (standards newResponsibility)
    <*> zipWithMaybeM
      (preparePractice modelName ctxrespPractice)
      (practices newResponsibility)
      (practices <$> oldResponsibility)

preparePractice ::
  Text ->
  ContextSWEPractice ->
  SWEPractice TextAtom ->
  Maybe (SWEPractice LLMPromptResultText) ->
  TraverseAction (SWEPractice LLMPromptParameters)
preparePractice modelName (ContextSWEPractice ctx) new oldMay =
  SWEPractice <$> prepareAtom modelName ctx (getPractice new) (getPractice <$> oldMay)

prepareSpokenLanguage ::
  Text ->
  ContextSpokenLanguage ->
  SpokenLanguage TextAtom ->
  Maybe (SpokenLanguage LLMPromptResultText) ->
  TraverseAction (SpokenLanguage LLMPromptParameters)
prepareSpokenLanguage modelName (ContextSpokenLanguage ctx) new oldMay =
  SpokenLanguage <$> prepareAtom modelName ctx (getLanguage new) (getLanguage <$> oldMay)

prepareSelfEmployed ::
  Text ->
  ContextSelfEmployed ->
  SelfEmployed TextAtom ->
  Maybe (SelfEmployed LLMPromptResultText) ->
  TraverseAction (SelfEmployed LLMPromptParameters)
prepareSelfEmployed modelName ContextSelfEmployed {..} new oldMay =
  SelfEmployed
    <$> zipWithMaybeM
      (prepareCustomer modelName ctxseCustomer)
      (customers new)
      (customers <$> oldMay)

prepareCustomer ::
  Text ->
  ContextCustomer ->
  Customer TextAtom ->
  Maybe (Customer LLMPromptResultText) ->
  TraverseAction (Customer LLMPromptParameters)
prepareCustomer modelName ContextCustomer {..} newCustomer oldCustomer =
  Customer
    <$> prepareAtom
      modelName
      ctxcuCustomerDescription
      (customerDescription newCustomer)
      (customerDescription <$> oldCustomer)
    <*> zipWithMaybeM
      (prepareResponsibility modelName ctxcuResponsibility)
      (customerResponsibilities newCustomer)
      (customerResponsibilities <$> oldCustomer)

prepareSection ::
  Text ->
  ContextSection t1 ->
  Section t2 TextAtom ->
  Maybe (Section b1 LLMPromptResultText) ->
  (t1 -> t2 -> Maybe b1 -> TraverseAction b2) ->
  TraverseAction (Section b2 LLMPromptParameters)
prepareSection modelName ContextSection {..} newSection oldSectionMaybe f = do
  newHeading <-
    prepareAtom
      modelName
      headingContext
      (sectionHeading newSection)
      (sectionHeading <$> oldSectionMaybe)
  newBody <-
    f bodyContext (sectionBody newSection) (sectionBody <$> oldSectionMaybe)
  pure $ Section newHeading newBody

mkSystemMessage :: Text -> Message
mkSystemMessage content =
  Message
    { role = System,
      images = Nothing,
      tool_calls = Nothing,
      thinking = Nothing,
      ..
    }

mkUserMessage :: Text -> Message
mkUserMessage content =
  Message
    { role = User,
      images = Nothing,
      tool_calls = Nothing,
      thinking = Nothing,
      ..
    }

chatConfig :: Text -> NonEmpty Message -> ChatOps
chatConfig modelName messages =
  ChatOps
    { tools = Nothing,
      format = Nothing,
      stream = Nothing,
      keepAlive = Nothing,
      options =
        Just
          defaultModelOptions
            { temperature = Just 0.1,
              topP = Just 0.5
            },
      think = Nothing,
      ..
    }

-- this is not your usual zip, it keeps calling f with a Nothing second parameter if [b] is shorter.
-- >>> zipWithMaybeM (\x _ -> pure x) [1] (Just [])
-- [1]
zipWithMaybeM :: (Monad m) => (a -> Maybe b -> m c) -> [a] -> Maybe [b] -> m [c]
zipWithMaybeM f as Nothing = mapM (`f` Nothing) as
zipWithMaybeM f as (Just bs) =
  zipWithM f as (Prelude.map Just bs ++ Prelude.repeat Nothing)
