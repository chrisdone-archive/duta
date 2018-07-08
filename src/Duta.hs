{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duta
    ( start
    ) where

import qualified Data.ByteString.Lazy.Builder as L
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.ByteString.Builder as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as Net
import qualified Data.Conduit.Network.TLS as TLS
import           Data.Monoid

--------------------------------------------------------------------------------
-- Constants

start :: IO ()
start =
  TLS.runTCPServerStartTLS
    (TLS.tlsConfig "127.0.0.1" smtpPort certFile keyFile)
    (\(appData, _startTls) -> do
       putStrLn "Got connection!"
       reply appData ServiceReady
       C.runConduit (Net.appSource appData .| CL.mapM_ print))

reply :: Net.AppData -> Reply -> IO ()
reply appData rep =
  C.runConduit
    (C.yield (buildReply rep <> "\n") .| CB.builderToByteString .| Net.appSink appData)

data Reply =
  ServiceReady

buildReply :: Reply -> L.Builder
buildReply =
  \case
    ServiceReady -> L.intDec 220

--------------------------------------------------------------------------------
-- Constants

certFile :: FilePath
certFile = "data/server.crt"

keyFile :: FilePath
keyFile = "data/server.key"

-- | https://www.mailgun.com/blog/25-465-587-what-port-should-i-use
smtpPort :: Int
smtpPort = 5870
