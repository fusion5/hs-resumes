{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module DocumentTypes.CV (module DocumentTypes.CV) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor
import Data.ByteString
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BC
import Data.Ollama.Chat
import Data.String (IsString (..))
import Data.Text
import Data.Text.Encoding qualified as T
import GHC.Generics

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

data ProgrammingLanguage = ProgrammingLanguage
  { -- | e.g. nf-mysql, nf-c, nf-haskell...
    plCSSIcon :: Maybe Text,
    unProgrammingLanguage :: Text
  }
  deriving (Show, Eq, Generic)

data Technology = Technology
  { -- | e.g. nf-mysql, nf-c, nf-haskell...
    techCSSIcon :: Maybe Text,
    unTechnology :: Text
  }
  deriving (Show, Eq, Generic)

data Standard = Standard
  { -- | e.g. nf-mysql, nf-c, nf-haskell...
    standardCSSIcon :: Maybe Text,
    unStandard :: Text
  }
  deriving (Show, Eq, Generic)

newtype SWEPractice a = SWEPractice {getPractice :: a}
  deriving (Show, Eq, IsString, Functor, Foldable, Traversable, Generic)

newtype SpokenLanguage a = SpokenLanguage
  { getLanguage :: a
  }
  deriving (Show, Eq, IsString, Functor, Foldable, Traversable, Generic)

newtype Heading a = Heading a
  deriving (Show, Eq, IsString, Functor, Foldable, Traversable, Generic)

data TimePoint = TimePoint
  { year :: Integer,
    month :: Integer
  }
  deriving (Show, Eq, Generic)

data Responsibility a = Responsibility
  { description :: a,
    languages :: [ProgrammingLanguage],
    technologies :: [Technology],
    standards :: [Standard],
    practices :: [SWEPractice a]
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data Customer a = Customer
  { customerDescription :: a,
    customerResponsibilities :: [Responsibility a]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data Employee a
  = Employee
  { employerDescription :: a,
    employerResponsibilities :: [Responsibility a]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

newtype SelfEmployed a = SelfEmployed
  { customers :: [Customer a]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data ExperienceType a
  = EmployeeExperience (Employee a)
  | SelfEmployedExperience (SelfEmployed a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data WorkExperience a
  = WorkExperience
  { startTime :: TimePoint,
    endTime :: TimePoint,
    position :: Text,
    employer :: Text,
    workLocation :: a,
    job :: ExperienceType a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data EducationDescription a = EducationDescription
  { eduDescription :: a,
    eduLanguages :: [ProgrammingLanguage],
    eduTechnologies :: [Technology],
    eduStandards :: [Standard],
    eduPractices :: [SWEPractice a]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data Education a = Education
  { eduStartTime :: TimePoint,
    eduEndTime :: TimePoint,
    eduDegree :: a,
    eduInstitution :: Text,
    eduLocation :: a,
    eduDescriptions :: [EducationDescription a]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data Awards a = Awards
  { awardTime :: TimePoint,
    awardDescription :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data Section b a = Section
  { sectionHeading :: a,
    sectionBody :: b
  }
  deriving (Eq, Show, Foldable, Functor, Traversable, Generic)

instance Bifunctor Section where
  bimap :: (a -> b) -> (c -> d) -> Section a c -> Section b d
  bimap f g (Section {..}) =
    Section
      { sectionHeading = g sectionHeading,
        sectionBody = f sectionBody
      }

data CV a = CV
  { currentLocation :: a,
    cvSummary :: Section a a,
    cvObjective :: Section a a,
    cvProgrammingLanguages :: Section [ProgrammingLanguage] a,
    cvTechnologies :: Section [Technology] a,
    cvStandards :: Section [Standard] a,
    cvPractices :: Section [SWEPractice a] a,
    cvSpokenLanguages :: Section [SpokenLanguage a] a,
    cvWorkExperience :: Section [WorkExperience a] a,
    cvEducation :: Section [Education a] a,
    cvAwards :: Section [Awards a] a
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (CV a))

instance Functor CV where
  fmap :: (a -> b) -> CV a -> CV b
  fmap f (CV {..}) =
    CV
      { currentLocation = f currentLocation,
        cvSummary = bimap f f cvSummary,
        cvObjective = bimap f f cvObjective,
        cvProgrammingLanguages = fmap f cvProgrammingLanguages,
        cvTechnologies = fmap f cvTechnologies,
        cvStandards = fmap f cvStandards,
        cvPractices = bimap (fmap (fmap f)) f cvPractices,
        cvSpokenLanguages = bimap (fmap (fmap f)) f cvSpokenLanguages,
        cvWorkExperience = bimap (fmap (fmap f)) f cvWorkExperience,
        cvEducation = bimap (fmap (fmap f)) f cvEducation,
        cvAwards = bimap (fmap (fmap f)) f cvAwards
      }

instance Foldable CV where
  foldr :: (a -> b -> b) -> b -> CV a -> b
  foldr f z cv =
    foldPlain (cvSummary cv) $
      foldPlain (cvObjective cv) $
        foldOpaque (cvProgrammingLanguages cv) $
          foldOpaque (cvTechnologies cv) $
            foldOpaque (cvStandards cv) $
              foldNested (cvPractices cv) $
                foldNested (cvSpokenLanguages cv) $
                  foldNested (cvWorkExperience cv) $
                    foldNested (cvEducation cv) $
                      foldNested (cvAwards cv) z
    where
      foldOpaque (Section h _) = f h
      foldPlain (Section h b) acc = f h (f b acc)
      foldNested (Section h b) acc = f h (Prelude.foldr (flip (Prelude.foldr f)) acc b)

instance Traversable CV where
  traverse :: (Applicative f) => (a -> f b) -> CV a -> f (CV b)
  traverse f cv =
    CV
      <$> f (currentLocation cv)
      <*> traversePlain (cvSummary cv)
      <*> traversePlain (cvObjective cv)
      <*> traverseOpaque (cvProgrammingLanguages cv)
      <*> traverseOpaque (cvTechnologies cv)
      <*> traverseOpaque (cvStandards cv)
      <*> traverseNested (cvPractices cv)
      <*> traverseNested (cvSpokenLanguages cv)
      <*> traverseNested (cvWorkExperience cv)
      <*> traverseNested (cvEducation cv)
      <*> traverseNested (cvAwards cv)
    where
      traversePlain (Section h b) = Section <$> f h <*> f b
      traverseOpaque (Section h b) = Section <$> f h <*> pure b
      traverseNested (Section h b) = Section <$> f h <*> traverse (traverse f) b

-- Context types provide context for the CV structure. They are nested to make their definition
-- more intuitive. We basically zip ContextCV and CV in the context of PromptResolution module.
-- Along the context we also specify the transform action to pass to the LLM
data LLMInstructions = LLMInstructions
  { llmContext :: Text,
    llmImperative :: Text
  }
  deriving (Eq, Show)

mkInstr :: Text -> Text -> LLMInstructions
mkInstr llmContext llmImperative = LLMInstructions {..}

newtype ContextSWEPractice = ContextSWEPractice
  { swePracticeInstructions :: LLMInstructions
  }
  deriving (Eq, Show)

newtype ContextSpokenLanguage = ContextSpokenLanguage
  { spokenLanguageInstructions :: LLMInstructions
  }
  deriving (Eq, Show)

data ContextEmployee = ContextEmployee
  { ctxeEmployerDescription :: LLMInstructions,
    ctxeResponsibilities :: ContextResponsibility
  }
  deriving (Eq, Show)

data ContextResponsibility = ContextResponsibility
  { ctxrespDescription :: LLMInstructions,
    ctxrespPractice :: ContextSWEPractice
  }
  deriving (Eq, Show)

data ContextCustomer = ContextCustomer
  { ctxcuCustomerDescription :: LLMInstructions,
    ctxcuResponsibility :: ContextResponsibility
  }
  deriving (Eq, Show)

newtype ContextSelfEmployed = ContextSelfEmployed
  { ctxseCustomer :: ContextCustomer
  }
  deriving (Eq, Show)

-- The sum type of ExperienceType becomes a product type because in the context we must always
-- specify both cases
data ContextExperienceType
  = ContextExperienceType
  { ctxEmployee :: ContextEmployee,
    ctxSelfEmployed :: ContextSelfEmployed
  }
  deriving (Eq, Show)

data ContextWorkExperience = ContextWorkExperience
  { ctxweLocation :: LLMInstructions, -- E.g. this is the location of a job
    ctxweJob :: ContextExperienceType
  }
  deriving (Eq, Show)

data ContextSection a = ContextSection
  { headingContext :: LLMInstructions,
    bodyContext :: a
  }
  deriving (Eq, Show)

data ContextEducationDescription = ContextEducationDescription
  { ctxeddDescription :: LLMInstructions,
    ctxeddSWEPractices :: ContextSWEPractice
  }
  deriving (Eq, Show)

data ContextEducation = ContextEducation
  { ctxeduDegree :: LLMInstructions,
    ctxeduLocation :: LLMInstructions,
    ctxeduDescription :: ContextEducationDescription
  }
  deriving (Eq, Show)

newtype ContextAward = ContextAward
  { ctxAward :: LLMInstructions
  }
  deriving (Eq, Show)

data ContextCV = ContextCV
  { ctxModelName :: Text,
    ctxCVContext :: LLMInstructions,
    ctxCVLocation :: LLMInstructions,
    ctxSummary :: ContextSection LLMInstructions,
    ctxObjective :: ContextSection LLMInstructions,
    ctxProgrammingLanguage :: ContextSection (),
    ctxTechnologies :: ContextSection (),
    ctxStandards :: ContextSection (),
    ctxPractices :: ContextSection ContextSWEPractice,
    ctxSpokenLanguages :: ContextSection ContextSpokenLanguage,
    ctxWorkExperience :: ContextSection ContextWorkExperience,
    ctxEducation :: ContextSection ContextEducation,
    ctxAwards :: ContextSection ContextAward
  }
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec ContextCV)

instance HasCodec ContextCV where
  codec =
    object "ContextCV" $
      ContextCV
        <$> requiredField "modelName" "The LLM model to use in ollama"
          .= ctxModelName
        <*> requiredField "contextCV" "The current location"
          .= ctxCVContext
        <*> requiredField "contextLocation" "The context for the CV"
          .= ctxCVLocation
        <*> requiredField "contextSummary" "The summary section context"
          .= ctxSummary
        <*> requiredField "contextObjective" "The objective section context"
          .= ctxObjective
        <*> requiredField "contextProgrammingLanguages" "The PL section context"
          .= ctxProgrammingLanguage
        <*> requiredField "contextTechnologies" "The technologies section context"
          .= ctxTechnologies
        <*> requiredField "contextStandards" "The standards section context"
          .= ctxStandards
        <*> requiredField "contextPractices" "The SWE practices section context"
          .= ctxPractices
        <*> requiredField "contextSpokenLanguages" "Spoken language section context"
          .= ctxSpokenLanguages
        <*> requiredField "contextWorkExperience" "Work experience section context"
          .= ctxWorkExperience
        <*> requiredField "contextEducation" "Education section context"
          .= ctxEducation
        <*> requiredField "contextAwards" "Awards section context"
          .= ctxAwards

instance (HasCodec a) => HasCodec (ContextSection a) where
  codec =
    object "ContextSection" $
      ContextSection
        <$> requiredField "headingContext" "The heading context" .= headingContext
        <*> requiredField "bodyContext" "The body context" .= bodyContext

instance {-# OVERLAPPING #-} HasCodec (ContextSection ()) where
  codec =
    object "ContextSection" $
      ContextSection
        <$> requiredField "headingContext" "The section context" .= headingContext
        <*> pure ()

instance HasCodec ContextAward where
  codec =
    object "ContextAward" $
      ContextAward
        <$> requiredField "contextAward" "The award instructions" .= ctxAward

instance HasCodec ContextEducation where
  codec =
    object "ContextEducation" $
      ContextEducation
        <$> requiredField "contextDegree" "The degree instructions" .= ctxeduDegree
        <*> requiredField "contextLocation" "The location instructions" .= ctxeduLocation
        <*> requiredField "contextDescription" "The description instructions" .= ctxeduDescription

instance HasCodec ContextEducationDescription where
  codec =
    object "ContextEducation" $
      ContextEducationDescription
        <$> requiredField "experience" "The educational experience" .= ctxeddDescription
        <*> requiredField "swePractices" "The SWE practices developed in this experience" .= ctxeddSWEPractices

instance HasCodec ContextExperienceType where
  codec =
    object "ContextExperienceType" $
      ContextExperienceType
        <$> requiredField "contextEmployee" "The employee context" .= ctxEmployee
        <*> requiredField "contextSelfEmployed" "The self employed context" .= ctxSelfEmployed

instance HasCodec ContextEmployee where
  codec =
    object "Context employee" $
      ContextEmployee
        <$> requiredField "employerDescription" "Employer description" .= ctxeEmployerDescription
        <*> requiredField "responsibilities" "Employer responsibilities" .= ctxeResponsibilities

instance HasCodec ContextSpokenLanguage where
  codec =
    object "Spoken language" $
      ContextSpokenLanguage
        <$> requiredField "spokenLanguageInstructions" "Spoken language" .= spokenLanguageInstructions

instance HasCodec ContextResponsibility where
  codec =
    object "ContextResponsibility" $
      ContextResponsibility
        <$> requiredField "responsibility" "The description" .= ctxrespDescription
        <*> requiredField "swePractices" "The SWE practices" .= ctxrespPractice

instance HasCodec ContextCustomer where
  codec =
    object "ContextCustomer" $
      ContextCustomer
        <$> requiredField "customer" "The customer description" .= ctxcuCustomerDescription
        <*> requiredField "responsibilities" "The customer responsibilities" .= ctxcuResponsibility

instance HasCodec ContextSelfEmployed where
  codec =
    object "ContextSelfEmployed" $
      ContextSelfEmployed
        <$> requiredField "contextCustomer" "The customer context" .= ctxseCustomer

instance HasCodec ContextWorkExperience where
  codec =
    object "ContextWorkExperience" $
      ContextWorkExperience
        <$> requiredField "location" "The work experience location" .= ctxweLocation
        <*> requiredField "job" "The job experience location" .= ctxweJob

instance HasCodec ContextSWEPractice where
  codec =
    object "ContextSWEPractice" $
      ContextSWEPractice
        <$> requiredField "swePractice" "The SWE Practice instruction" .= swePracticeInstructions

instance HasCodec LLMInstructions where
  codec =
    object "LLMInstructions" $
      LLMInstructions
        <$> requiredField "context" "The context" .= llmContext
        <*> requiredField "imperative" "The instruction" .= llmImperative

instance (HasCodec a) => HasCodec (CV a) where
  codec =
    object "CV" $
      CV
        <$> requiredField "location" "Current Location" .= currentLocation
        <*> requiredField "summary" "Summary" .= cvSummary
        <*> requiredField "objective" "Objective" .= cvObjective
        <*> requiredField "programmingLanguages" "Programming Languages" .= cvProgrammingLanguages
        <*> requiredField "technologies" "Technologies" .= cvTechnologies
        <*> requiredField "standards" "Standards" .= cvStandards
        <*> requiredField "practices" "SWE Practices" .= cvPractices
        <*> requiredField "spokenLanguages" "Spoken Languages" .= cvSpokenLanguages
        <*> requiredField "workExperience" "Work Experience" .= cvWorkExperience
        <*> requiredField "education" "Education" .= cvEducation
        <*> requiredField "awards" "Awards" .= cvAwards

instance (HasCodec a, HasCodec b) => HasCodec (Section a b) where
  codec =
    object "Section" $
      Section
        <$> requiredField "heading" "Heading" .= sectionHeading
        <*> requiredField "body" "Body" .= sectionBody

instance (HasCodec a) => HasCodec (Awards a) where
  codec =
    object "Awards" $
      Awards
        <$> requiredField "time" "Award time" .= awardTime
        <*> requiredField "description" "Award description" .= awardDescription

instance HasCodec TimePoint where
  codec =
    object "TimePoint" $
      TimePoint
        <$> requiredField "year" "Year" .= year
        <*> requiredField "month" "Month" .= month

instance (HasCodec a) => HasCodec (Education a) where
  codec =
    object "Education" $
      Education
        <$> requiredField "start" "Start time" .= eduStartTime
        <*> requiredField "end" "End time" .= eduEndTime
        <*> requiredField "degree" "Degree" .= eduDegree
        <*> requiredField "institution" "Institution" .= eduInstitution
        <*> requiredField "location" "Location" .= eduLocation
        <*> requiredField "description" "Descriptions" .= eduDescriptions

instance (HasCodec a) => HasCodec (EducationDescription a) where
  codec =
    object "Education description" $
      EducationDescription
        <$> requiredField "description" "Description" .= eduDescription
        <*> requiredField "programmingLanguages" "ProgrammingLanguages" .= eduLanguages
        <*> requiredField "technologies" "Technologies" .= eduTechnologies
        <*> requiredField "standards" "Standards" .= eduStandards
        <*> requiredField "practices" "Practices" .= eduPractices

instance (HasCodec a) => HasCodec (SWEPractice a) where
  codec =
    object "SWEPractice" $
      SWEPractice
        <$> requiredField "practice" "Software Engineering Practice" .= getPractice

instance (HasCodec a) => HasCodec (SpokenLanguage a) where
  codec =
    object "SpokenLanguage" $
      SpokenLanguage
        <$> requiredField "language" "Spoken Language" .= getLanguage

instance HasCodec Standard where
  codec = disjointMatchChoiceCodec simpleCodec complexCodec decide
    where
      decide value@(Standard Nothing _) = Left value
      decide value@(Standard {}) = Right value
      simpleCodec = dimapCodec (Standard Nothing) unStandard textCodec
      complexCodec =
        object "Standard" $
          Standard
            <$> optionalField "icon" "Icon, optional" .= standardCSSIcon
            <*> requiredField "text" "Standard" .= unStandard

instance HasCodec Technology where
  codec = disjointMatchChoiceCodec simpleCodec complexCodec decide
    where
      decide value@(Technology Nothing _) = Left value
      decide value@(Technology {}) = Right value
      simpleCodec = dimapCodec (Technology Nothing) unTechnology textCodec
      complexCodec =
        object "Technology" $
          Technology
            <$> optionalField "icon" "Icon, optional" .= techCSSIcon
            <*> requiredField "text" "Programming Language" .= unTechnology

instance HasCodec ProgrammingLanguage where
  codec = disjointMatchChoiceCodec simpleCodec complexCodec decide
    where
      decide value@(ProgrammingLanguage Nothing _) = Left value
      decide value@(ProgrammingLanguage {}) = Right value
      simpleCodec = dimapCodec (ProgrammingLanguage Nothing) unProgrammingLanguage textCodec
      complexCodec =
        object "ProgrammingLanguage" $
          ProgrammingLanguage
            <$> optionalField "icon" "Icon, optional" .= plCSSIcon
            <*> requiredField "text" "Programming Language" .= unProgrammingLanguage

-- dimapCodec Standard unStandard textCodec

instance (HasCodec a) => HasCodec (WorkExperience a) where
  codec =
    object "WorkExperience" $
      WorkExperience
        <$> requiredField "start" "Start" .= startTime
        <*> requiredField "end" "End" .= endTime
        <*> requiredField "position" "Position" .= position
        <*> requiredField "employer" "Employer" .= employer
        <*> requiredField "location" "Work Location" .= workLocation
        <*> requiredField "job" "Job" .= job

instance (HasCodec a) => HasCodec (ExperienceType a) where
  codec = disjointMatchChoiceCodec employeeCodec selfEmployedCodec decide
    where
      decide :: ExperienceType a -> Either (ExperienceType a) (ExperienceType a)
      decide experienceType@(EmployeeExperience {}) = Left experienceType
      decide experienceType@(SelfEmployedExperience {}) = Right experienceType
      employeeCodec =
        object "EmployeeExperience" $
          EmployeeExperience
            <$> requiredField "employee" "Employee" .= unEE
      selfEmployedCodec =
        object "SelfEmployedExperience" $
          SelfEmployedExperience
            <$> requiredField "selfEmployed" "Self employed" .= unSEE
      unEE (EmployeeExperience e) = e
      unEE _ = error "logical error"
      unSEE (SelfEmployedExperience e) = e
      unSEE _ = error "logical error"

instance (HasCodec a) => HasCodec (SelfEmployed a) where
  codec =
    object "SelfEmployed" $
      SelfEmployed <$> requiredField "customers" "Customers" .= customers

instance (HasCodec a) => HasCodec (Customer a) where
  codec =
    object "Customer" $
      Customer
        <$> requiredField "customerDescription" "Description" .= customerDescription
        <*> requiredField "responsibilities" "Responsibilities" .= customerResponsibilities

instance (HasCodec a) => HasCodec (Responsibility a) where
  codec =
    object "Responsibility" $
      Responsibility
        <$> requiredField "description" "Description" .= description
        <*> requiredField "programmingLanguages" "Programming Languages" .= languages
        <*> requiredField "technologies" "Technologies" .= technologies
        <*> requiredField "standards" "Standards" .= standards
        <*> requiredField "practices" "Practices" .= practices

instance (HasCodec a) => HasCodec (Employee a) where
  codec =
    object "Employee" $
      Employee
        <$> requiredField "employerDescription" "Description" .= employerDescription
        <*> requiredField "responsibilities" "Responsibilities" .= employerResponsibilities

instance HasCodec TextAtom where
  codec = dimapCodec TextAtom getText textCodec

instance HasCodec LLMPromptResultText where
  codec =
    object "LLMPromptResultText" $
      LLMPromptResultText
        <$> requiredField "promptResult" "Prompt Result" .= getPromptResultText
        <*> requiredField "hash" "MD5 Hash" .= inputHashMD5

instance HasCodec MD5Hash where
  -- FIXME: decodeUtf8 is unsafe, should use decodeUtf8'
  codec = dimapCodec (MD5Hash . decodeBase64) (T.decodeUtf8 . B64.encode . getHash) textCodec
    where
      decodeBase64 text =
        case B64.decode (T.encodeUtf8 text) of
          Left err -> error $ "Invalid Base64: " ++ err
          Right bs -> bs
