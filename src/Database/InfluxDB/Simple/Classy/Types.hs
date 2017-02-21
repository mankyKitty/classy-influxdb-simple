{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Database.InfluxDB.Simple.Classy.Types
  ( InfluxRqType (..)
  , ToLine (..)
  , IsDb (..)
  , InfluxQuery (..)
  , InfluxDBConfig (..)
  , HasInfluxDBConfig (..)
  , InfluxDbError (..)
  , AsInfluxDbError (..)
  , CanInflux
  , basicInfluxOpts
  , rqWinCode
  ) where

import           Control.Exception      (SomeException)
import           Control.Lens           (makeClassy, makeClassyPrisms, (.~),
                                         (?~), (^.))
import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO)

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS8

import           Data.Function          ((&))

import           Data.Text              (Text)

import           Data.Vector            (Vector)
import qualified Data.Vector            as V

import           GHC.Word               (Word32)

import           Network.Wreq           (Options)
import qualified Network.Wreq           as W

data InfluxRqType
  = Write
  | Query
  | QueryCSV
  deriving Show

rqWinCode :: InfluxRqType -> Int
rqWinCode Write    = 204
rqWinCode Query    = 200
rqWinCode QueryCSV = 200

class ToLine a where
  toLine :: a -> ByteString

instance ToLine a => ToLine [a] where
  toLine = BS8.unlines . fmap toLine

instance ToLine a => ToLine (Vector a) where
  toLine = BS8.unlines . V.toList . fmap toLine

class IsDb a where
  toDbName :: a -> Text

newtype InfluxQuery = InfluxQuery ByteString
  deriving (Eq, Show)

data InfluxDBConfig = InfluxDBConfig
  { _idbUser :: ByteString
  , _idbPass :: ByteString
  , _idbHost :: ByteString
  , _idbPort :: Int
  }
makeClassy ''InfluxDBConfig

data InfluxDbError
  = CommsOrInfluxError ByteString
  | ParseError ByteString
  | UnknownError InfluxRqType SomeException
  deriving Show
makeClassyPrisms ''InfluxDbError

type CanInflux m e =
  ( AsInfluxDbError e
  , MonadError e m
  , MonadIO m
  )

basicInfluxOpts
  :: IsDb a
  => Options
  -> InfluxDBConfig
  -> a
  -> Options
basicInfluxOpts o env db = o
  & W.param "db" .~ [toDbName db]
  & W.auth ?~ W.basicAuth user' pass'
  where
    user' = env ^. idbUser
    pass' = env ^. idbPass
