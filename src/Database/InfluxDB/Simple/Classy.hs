{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Database.InfluxDB.Simple.Classy
  ( writeData
  , queryData
  , queryDataToCSV
  ) where

import           Control.Lens                          (to, view, (.~), (^.))
import           Control.Monad.Error.Lens              (throwing)

import qualified Data.ByteString.Char8                 as BS8
import           Data.ByteString.Lazy                  (ByteString)
import           Data.ByteString.Lens                  (unpackedChars)

import           Data.Function                         ((&))
import           Data.Monoid                           ((<>))

import qualified Data.Text.Encoding                    as Text

import qualified Network.Wreq                          as W

import           Database.InfluxDB.Simple.Classy.Types (AsInfluxDbError (..),
                                                        CanInflux,
                                                        HasInfluxDBConfig (..),
                                                        InfluxDBConfig,
                                                        InfluxDbError (..),
                                                        InfluxQuery (..),
                                                        InfluxRqType (..),
                                                        IsDb (..), ToLine (..),
                                                        basicInfluxOpts,
                                                        rqWinCode)

import           Data.Aeson                            (Value)
import qualified Data.Aeson                            as A

import           Control.Concurrent.Async.Either (runAsyncToE)

writeUrl,queryUrl,baseUrl :: HasInfluxDBConfig c => c -> String
baseUrl c  = c ^. idbHost . unpackedChars <> ":" <> c ^. idbPort . to show
writeUrl c = baseUrl c <> "/" <> "write"
queryUrl c = baseUrl c <> "/" <> "query"

handleSuccess
  :: CanInflux m e
  => InfluxRqType
  -> (W.Response ByteString -> m a)
  -> W.Response ByteString
  -> m a
handleSuccess rqT f r
  | r ^. W.responseStatus . W.statusCode . to (== rqWinCode rqT)
    = f r
  | otherwise
    = throwing _CommsOrInfluxError $ r ^. W.responseStatus . W.statusMessage

runInflux
  :: CanInflux m e
  => IO (W.Response ByteString)
  -> InfluxRqType
  -> (W.Response ByteString -> m a)
  -> m a
runInflux act rq resFn = do
  r <- runAsyncToE act (UnknownError rq)
  either (throwing _InfluxDbError) (handleSuccess rq resFn) r

writeData
  :: ( CanInflux m e
     , IsDb db
     , ToLine l
     )
  => InfluxDBConfig
  -> db
  -> l
  -> m ()
writeData c db l =
  runInflux (W.postWith o (writeUrl c) (toLine l)) Write win
  where
    win = pure . const ()
    o = basicInfluxOpts W.defaults c db

queryDataToCSV
  :: ( CanInflux m e
     , IsDb db
     )
  => InfluxDBConfig
  -> db
  -> InfluxQuery
  -> m ByteString
queryDataToCSV c db (InfluxQuery q) =
  runInflux (W.getWith o (queryUrl c)) QueryCSV (pure . view W.responseBody)
  where
    o = basicInfluxOpts W.defaults c db
      & W.param "q" .~ [Text.decodeUtf8 q]
      & W.header "Accept" .~ ["application/csv"]

queryData
  :: ( CanInflux m e
     , IsDb db
     )
  => InfluxDBConfig
  -> db
  -> InfluxQuery
  -> m Value
queryData c db (InfluxQuery q) =
  runInflux (W.getWith o (queryUrl c)) Query success
  where
    success r = either (throwing _ParseError . BS8.pack)
      pure $ r ^. W.responseBody . to A.eitherDecode

    o = basicInfluxOpts W.defaults c db
      & W.param "q" .~ [Text.decodeUtf8 q]
