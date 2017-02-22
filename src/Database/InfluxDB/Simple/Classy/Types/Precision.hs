{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Database.InfluxDB.Simple.Classy.Types.Precision
  ( Precision (..)
  , AsPrecision (..)
  ) where

import           Control.Lens         (Iso', Prism', makeClassyPrisms, preview,
                                       prism', re, view, (^?))

import           Data.ByteString      (ByteString)
import           Data.ByteString.Lens (packedChars, unpackedChars)
import           Data.Text            (Text)
import           Data.Text.Lens       (packed, unpacked)

-- |
-- Precision of the timestamp in the writes or queries to Influx
--  [n,u,ms,s,m,h]
data Precision
  = Nanoseconds
  | Microseconds
  | Milliseconds
  | Seconds
  | Minutes
  | Hours
  deriving (Eq, Ord, Enum)
makeClassyPrisms ''Precision

instance AsPrecision String where
  _Precision = prism'
    (\case
      Nanoseconds  -> "n"
      Microseconds -> "u"
      Milliseconds -> "ms"
      Seconds      -> "s"
      Minutes      -> "m"
      Hours        -> "h"
    )
    (\case
       "n"  -> Just Nanoseconds
       "u"  -> Just Microseconds
       "ms" -> Just Milliseconds
       "s"  -> Just Seconds
       "m"  -> Just Minutes
       "h"  -> Just Hours
       _    -> Nothing
    )

{-# INLINABLE pris' #-}
pris' :: Iso' a String -> Iso' String a -> Prism' a Precision
pris' f t = prism' (view (re _Precision . t)) (preview (f . _Precision))

instance AsPrecision Text where
  _Precision = pris' unpacked packed

instance AsPrecision ByteString where
  _Precision = pris' unpackedChars packedChars

instance Show Precision where
  show = view (re _Precision)
