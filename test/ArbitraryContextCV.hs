module ArbitraryContextCV () where

import DocumentTypes
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances ()

instance Arbitrary ContextCV where
  arbitrary =
    ContextCV
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
      <*> arbitrary
      <*> arbitrary

instance (Arbitrary a) => Arbitrary (ContextSection a) where
  arbitrary = ContextSection <$> arbitrary <*> arbitrary

instance Arbitrary LLMInstructions where
  arbitrary = LLMInstructions <$> arbitrary <*> arbitrary

instance Arbitrary ContextAward where
  arbitrary = ContextAward <$> arbitrary

instance Arbitrary ContextEducation where
  arbitrary = ContextEducation <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ContextSWEPractice where
  arbitrary = ContextSWEPractice <$> arbitrary

instance Arbitrary ContextWorkExperience where
  arbitrary = ContextWorkExperience <$> arbitrary <*> arbitrary

instance Arbitrary ContextExperienceType where
  arbitrary = ContextExperienceType <$> arbitrary <*> arbitrary

instance Arbitrary ContextEmployee where
  arbitrary = ContextEmployee <$> arbitrary <*> arbitrary

instance Arbitrary ContextSelfEmployed where
  arbitrary = ContextSelfEmployed <$> arbitrary

instance Arbitrary ContextCustomer where
  arbitrary = ContextCustomer <$> arbitrary <*> arbitrary

instance Arbitrary ContextResponsibility where
  arbitrary = ContextResponsibility <$> arbitrary <*> arbitrary

instance Arbitrary ContextEducationDescription where
  arbitrary = ContextEducationDescription <$> arbitrary <*> arbitrary

instance Arbitrary ContextSpokenLanguage where
  arbitrary = ContextSpokenLanguage <$> arbitrary
