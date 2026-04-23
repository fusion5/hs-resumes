{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module ArbitraryCV () where

import Data.Text
import DocumentTypes
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances ()

instance (Arbitrary a, Arbitrary b) => Arbitrary (Section b a) where
  arbitrary = Section <$> arbitrary <*> arbitrary
  shrink _ = []

instance (Arbitrary a) => Arbitrary (CV a) where
  arbitrary :: Gen (CV a)
  arbitrary =
    CV
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving via (Text) instance Arbitrary TextAtom

instance Arbitrary ProgrammingLanguage where
  arbitrary = ProgrammingLanguage <$> arbitrary <*> arbitrary

instance Arbitrary Technology where
  arbitrary = Technology <$> arbitrary <*> arbitrary

instance Arbitrary Standard where
  arbitrary = Standard <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Awards a) where
  arbitrary = Awards <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Arbitrary a) => Arbitrary (Education a) where
  arbitrary =
    Education
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance (Arbitrary a) => Arbitrary (SWEPractice a) where
  arbitrary = SWEPractice <$> arbitrary
  shrink = genericShrink

instance (Arbitrary a) => Arbitrary (SpokenLanguage a) where
  arbitrary = SpokenLanguage <$> arbitrary

instance (Arbitrary a) => Arbitrary (EducationDescription a) where
  arbitrary =
    EducationDescription
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Arbitrary a) => Arbitrary (WorkExperience a) where
  arbitrary =
    WorkExperience
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Arbitrary a) => Arbitrary (ExperienceType a) where
  arbitrary =
    oneof
      [ EmployeeExperience <$> arbitrary
      , SelfEmployedExperience <$> arbitrary
      ]

instance (Arbitrary a) => Arbitrary (Responsibility a) where
  arbitrary = Responsibility <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Employee a) where
  arbitrary = Employee <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Customer a) where
  arbitrary = Customer <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (SelfEmployed a) where
  arbitrary = SelfEmployed <$> arbitrary

instance Arbitrary TimePoint where
  arbitrary = TimePoint <$> arbitrary <*> arbitrary

instance Arbitrary MD5Hash where
  arbitrary = MD5Hash <$> arbitrary

instance Arbitrary LLMPromptResultText where
  arbitrary = LLMPromptResultText <$> arbitrary <*> arbitrary
