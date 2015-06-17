{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Parser.Tunecore (
  TunecoreRecord(..),
  parseTunecoreRecord,
  pDay
) where


import Data.Csv (Parser, FromNamedRecord(..), NamedRecord, (.:))
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Text (Text)
import Control.Applicative
import Data.Time.Calendar (Day, fromGregorian)
import Data.Monoid ((<>))

import qualified Data.ByteString.Char8 as B

pDash :: A.Parser Char
pDash = A.char '-'

-- update 2000 with current century
pDay :: A.Parser Day
pDay = fromGregorian <$> ((+2000) <$> A.decimal <* pDash)
                     <*> (A.decimal <* pDash)
                     <*> A.decimal

data TunecoreRecord = TunecoreRecord
  { tcSalesPeriod  :: !Day    -- "Sales Period"
  , tcPostedDate   :: !Day    -- "Posted Date"
  , tcStoreName    :: !Text   -- "Store Name"
  , tcCountry      :: !Text   -- "Country Of Sale"
  , tcArtist       :: !Text   -- "Artist"
  , tcReleaseType  :: !Text   -- "Release Type", Song or Album
  , tcReleaseTitle :: !Text   -- "Release Title"
  , tcSongTitle    :: !Text   -- "Song Title"
  , tcLabel        :: !Text   -- "Label"
  , tcUPC          :: !Text   -- "UPC"
  , tcOptionalUPC  :: Maybe Text   -- "Optional UPC"
  , tcSongID       :: !Text   -- "TC Song ID"
  , tcOptionalISRC :: Maybe Text   -- "Optional ISRC"
  , tcSalesType    :: !Text   -- "Sales Type"
  }
  deriving (Show)

(<?>) :: Functor f => f (Either b a) -> String -> f a
m <?> f = either (fail f) id <$> m

parseTunecoreRecord :: NamedRecord -> Parser TunecoreRecord
parseTunecoreRecord m = do
  let salesPeriodField = "Sales Period"
  salesPeriodRaw <- m .: salesPeriodField

  let dayFail c = "Could not parse " <> show c <> " day in Tunecore Sheet: " <> salesPeriodRaw

  salesPeriod  <- (A.parseOnly pDay <$> m .: salesPeriodField) <?> dayFail salesPeriodField
  postedDay    <- (A.parseOnly pDay <$> m .: "Posted Date") <?> dayFail ("Posted Date" :: B.ByteString)

  storeName    <- m .: "Store Name"
  country      <- m .: "Country Of Sale"
  artist       <- m .: "Artist"
  releaseType  <- m .: "Release Type"
  releaseTitle <- m .: "Release Title"
  songTitle    <- m .: "Song Title"
  label        <- m .: "Label"
  upc          <- m .: "UPC"
  optionalUPC  <- m .: "Optional UPC"
  songID       <- m .: "TC Song ID"
  optionalISRC <- m .: "Optional ISRC"
  salesType    <- m .: "Sales Type"

  return $ TunecoreRecord {
      tcSalesPeriod  = salesPeriod
    , tcPostedDate   = postedDay
    , tcStoreName    = storeName
    , tcCountry      = country
    , tcArtist       = artist
    , tcReleaseType  = releaseType
    , tcReleaseTitle = releaseTitle
    , tcSongTitle    = songTitle
    , tcLabel        = label
    , tcUPC          = upc
    , tcOptionalUPC  = optionalUPC
    , tcSongID       = songID
    , tcOptionalISRC = optionalISRC
    , tcSalesType    = salesType
  }

instance FromNamedRecord TunecoreRecord where
  parseNamedRecord = parseTunecoreRecord
