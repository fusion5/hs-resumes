module DocumentTypes.CoverLetter (module DocumentTypes.CoverLetter) where

import Data.Text

data Content a = Paragraph a | List [Content a]

data CoverLetter a = CoverLetter
  { currentLocation :: a,
    addressedTo :: Text,
    paragraphs :: [Content a]
  }
