{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Database.InfluxDB.Simple.Classy.Types.InfluxTimeStamp
  ( InfluxTimeStamp (..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Lens        (makeWrapped, (^.))
import           GHC.Generics        (Generic)

import           Data.Monoid         ((<>))

import           Data.Text           (Text)
import qualified Data.Text           as T
import Data.Text.Lens (packed,unpacked)

import           Data.Time           (UTCTime)
import qualified Data.Time           as T

import           Data.Aeson          (FromJSON (..), ToJSON (..))
import qualified Data.Aeson          as A
import           Data.Aeson.Types    (typeMismatch)
import           Data.Scientific     (FPFormat (Fixed), Scientific,
                                      formatScientific)

secondFmt :: String
secondFmt = "%s"

rfcFmt :: String
rfcFmt = "%FT%TZ"

-- |
-- Wrapper class for @UTCTime@ so we can provide some easier handling of
-- Influx timestamps coming in and out of the DB.
--
-- The @FromJSON@ instance can handle the following response granuality/format:
-- * seconds
-- * nanoseconds
-- * string format matching @2016-07-12T00:00:00Z@
--
-- The @ToJSON@ instance will print the time as epoch seconds
newtype InfluxTimeStamp = InfluxTimeStamp UTCTime
  deriving (Show, Eq, Generic)
makeWrapped ''InfluxTimeStamp

parseT
  :: String
  -> String
  -> Maybe UTCTime
parseT =
  T.parseTimeM False T.defaultTimeLocale

-- Time could be a timestamp "2016-07-12T00:00:00Z"
parseInfluxTSString
  :: Text
  -> Maybe UTCTime
parseInfluxTSString =
  parseT rfcFmt
    . T.unpack

-- Time could be a scientific nanosecond accurate value
parseInfluxTS
  :: Scientific
  -> Maybe UTCTime
parseInfluxTS =
  parseT secondFmt
   . formatScientific Fixed (Just 0)

toInfluxTS
  :: ( Show a
     , Monad m
     )
  => a
  -> Maybe UTCTime
  -> m InfluxTimeStamp
toInfluxTS a = maybe
  (fail ("Parse failed for InfluxTimeStamp: " <> show a))
  (pure . InfluxTimeStamp)

instance ToJSON InfluxTimeStamp where
  toEncoding (InfluxTimeStamp t) =
    toEncoding $ T.formatTime T.defaultTimeLocale secondFmt t

instance FromJSON InfluxTimeStamp where
  parseJSON (A.String t) = toInfluxTS t $ parseInfluxTSString t
  parseJSON (A.Number n) = toInfluxTS n $ nanos <|> seconds
    where
      nanos = parseInfluxTS (n / 1000000000)
      seconds = parseInfluxTS n

  parseJSON invalidVal = typeMismatch "InfluxTimeStamp" invalidVal
