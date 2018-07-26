{-# LANGUAGE RecordWildCards #-}
-- | Add timeout operations to conduit's network operations.

module Data.Conduit.Network.Timeout
  ( appSource
  , appSink
  , defaultConnector
  , Connector(..)
  , ConnectorCloseReason(..)
  ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit (await, ConduitT, yield)
import qualified Data.Streaming.Network as SN
import qualified GHC.Conc as Conc (yield)
import           System.Timeout (timeout)

data Connector ad = Connector
  { connectorAppData :: !ad
  -- ^ The app data.
  , connectorTimeout :: Int
  -- ^ Microseconds.
  }

data ConnectorCloseReason
  = Finished
  | ReadWriteTimeout
  deriving (Eq, Show)

defaultConnector :: ad -> Connector ad
defaultConnector ad =
  Connector {connectorAppData = ad, connectorTimeout = 1000000 * 10}

-- | Create a source from the app, but if a read takes longer than
-- @us@ microseconds, cancel and end the loop.
appSource ::
     (SN.HasReadWrite ad, MonadIO m) => Connector ad -> ConduitT i ByteString m ConnectorCloseReason
appSource Connector {..} = loop
  where
    read' = timeout connectorTimeout (SN.appRead connectorAppData)
    loop = do
      mbs <- liftIO read'
      case mbs of
        Nothing -> pure ReadWriteTimeout
        Just bs ->
          if S.null bs
            then pure Finished
            else do
              yield bs
              loop

-- | Create a sink into the app, but if it takes longer than @us@
-- microseconds to write to the socket, cancel and end the loop.
appSink ::
     (SN.HasReadWrite ad, MonadIO m) => Connector ad -> ConduitT ByteString o m ConnectorCloseReason
appSink Connector{..} = loop
  where
    loop = do
      md <- await
      case md of
        Nothing -> pure Finished
        Just d -> do
          mr <- liftIO (timeout connectorTimeout (SN.appWrite connectorAppData d) <* Conc.yield)
          case mr of
            Just {} -> loop
            Nothing -> pure ReadWriteTimeout
