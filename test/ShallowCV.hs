module ShallowCV () where

import DocumentTypes.CV
import DocumentTypes.Common
import ShallowEq

instance (ShallowEq a) => ShallowEq (CV a)

instance (ShallowEq a, ShallowEq b) => ShallowEq (Section b a)

instance (ShallowEq a) => ShallowEq (SWEPractice a)

instance (ShallowEq a) => ShallowEq (SpokenLanguage a)

instance (ShallowEq a) => ShallowEq (WorkExperience a)

instance (ShallowEq a) => ShallowEq (ExperienceType a)

instance (ShallowEq a) => ShallowEq (Employee a)

instance (ShallowEq a) => ShallowEq (SelfEmployed a)

instance (ShallowEq a) => ShallowEq (Customer a)

instance (ShallowEq a) => ShallowEq (Responsibility a)

instance (ShallowEq a) => ShallowEq (Education a)

instance (ShallowEq a) => ShallowEq (EducationDescription a)

instance (ShallowEq a) => ShallowEq (Awards a)

instance (ShallowEq a) => ShallowEq (Maybe a)

instance ShallowEq ()

instance ShallowEq Technology

instance ShallowEq TimePoint

instance ShallowEq ProgrammingLanguage

instance ShallowEq Standard

instance ShallowEq LLMPromptParameters where
  shallowEq (RunOllamaPrompt _ hash1) (RunOllamaPrompt _ hash2) = hash1 == hash2
  shallowEq (KeepCachedResult _ hash1) (KeepCachedResult _ hash2) = hash1 == hash2
  shallowEq _ _ = False
