{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module ArbitraryCoverLetter () where

import DocumentTypes.CoverLetter
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances ()

instance (Arbitrary a) => Arbitrary (CoverLetter a) where
  arbitrary :: Gen (CoverLetter a)
  arbitrary = CoverLetter <$> arbitrary <*> arbitrary <*> (Paragraph <$> arbitrary) -- arbitrary

instance (Arbitrary a) => Arbitrary (CoverLetterContent a) where
  arbitrary :: Gen (CoverLetterContent a)
  arbitrary = Paragraph <$> arbitrary
