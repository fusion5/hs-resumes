{-# LANGUAGE DerivingVia #-}

module DocumentTypes.CoverLetter (module DocumentTypes.CoverLetter) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics

data CoverLetter a = CoverLetter
  { currentLocation :: a,
    addressedTo :: Text,
    paragraphs :: CoverLetterContent a
  }
  deriving (Eq, Show, Generic, Functor)
  deriving (FromJSON, ToJSON) via (Autodocodec (CoverLetter a))

data CoverLetterContent a = Paragraph a | List [CoverLetterContent a]
  deriving (Eq, Show, Generic, Functor)
  deriving (FromJSON, ToJSON) via (Autodocodec (CoverLetterContent a))

instance (HasCodec a) => HasCodec (CoverLetterContent a) where
  codec =
    named "Content" $
      disjointMatchChoiceCodec paragraphCodec coverLetterListCodec decide
    where
      decide :: CoverLetterContent a -> Either (CoverLetterContent a) (CoverLetterContent a)
      decide x@(Paragraph {}) = Left x
      decide x@(List {}) = Right x

      paragraphCodec =
        object "Paragraph" $ Paragraph <$> requiredField "p" "Paragraph" .= unParagraph

      coverLetterListCodec =
        object "List" $ List <$> requiredField "list" "List" .= unList

      unParagraph (Paragraph x) = x
      unParagraph _ = error "logical error"
      unList (List xs) = xs
      unList _ = error "logical error"

instance (HasCodec a) => HasCodec (CoverLetter a) where
  codec =
    object "CoverLetter" $
      CoverLetter
        <$> requiredField "location" "Location" .= currentLocation
        <*> requiredField "to" "Addressed to" .= addressedTo
        <*> requiredField "paragraphs" "Contents" .= paragraphs
