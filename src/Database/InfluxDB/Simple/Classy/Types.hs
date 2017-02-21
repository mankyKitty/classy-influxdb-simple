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

-- |
-- Used to differentiate what action we were trying to perform on the InfluxDB
-- when an error occurred.
data InfluxRqType
  = Write
  | Query
  | QueryCSV
  deriving Show

-- Provides the HTTP status code that indicate a successful request from InfluxDB
rqWinCode :: InfluxRqType -> Int
rqWinCode Write    = 204
rqWinCode Query    = 200
rqWinCode QueryCSV = 200

-- |
-- Create an input line to be inserted into the InfluxDB. This is super loose right
-- now and provides no protection. If you do not comply with the specifications for
-- the InfluxDB <https://docs.influxdata.com/influxdb/v1.2/write_protocols/line_protocol_reference LineProtocol> then you're going to get
-- errors. Work in progress to make this a bit nicer and more helpful so the compiler
-- can prevent you from messing it up. :)
class ToLine a where
  toLine :: a -> ByteString

instance ToLine a => ToLine [a] where
  toLine = BS8.unlines . fmap toLine

instance ToLine a => ToLine (Vector a) where
  toLine = BS8.unlines . V.toList . fmap toLine

-- The name of the database where your series will be stored. This is not the
-- measurement name, just the database name.
class IsDb a where
  toDbName :: a -> Text

-- Query wrapper for an InfluxDb query. Only @SELECT@ queries are supported
-- by this API.
newtype InfluxQuery = InfluxQuery ByteString
  deriving (Eq, Show)

-- Details needed to authenticate with your InfluxDB, InfluxDB.Simple assumes
-- BasicAuth requirements for a DB.
data InfluxDBConfig = InfluxDBConfig
  { _idbUser :: ByteString
  , _idbPass :: ByteString
  , _idbHost :: ByteString
  , _idbPort :: Int
  }
makeClassy ''InfluxDBConfig

-- Influx Error types
data InfluxDbError
  = CommsOrInfluxError ByteString
  | ParseError ByteString
  | UnknownError InfluxRqType SomeException
  deriving Show
makeClassyPrisms ''InfluxDbError

-- TypeClass constraint to indicate that your classy-mtl can talk to and
-- handle errors from, your InfluxDB.
type CanInflux m e =
  ( AsInfluxDbError e
  , MonadError e m
  , MonadIO m
  )

-- The base Wreq options for talking to Influx, inflexible for now, but
-- will be changed in the future so alternative HTTP options can be provided
-- as needed.
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
