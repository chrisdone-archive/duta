{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Duta
import qualified Network.HaskellNet.SMTP.SSL as HaskellNet
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "Integration"
    (do it "Run server" (withServer (pure ()))
        it
          "Run server"
          (withServer
             (HaskellNet.doSMTPSTARTTLSWithSettings
                "precision"
                HaskellNet.defaultSettingsSMTPSTARTTLS
                  {HaskellNet.sslPort = 5870,HaskellNet.sslDisableCertificateValidation = True}
                (\c -> do putStrLn "Connected to server..."
                          HaskellNet.sendPlainTextMail
                            "receiver@127.0.0.1"
                            "sender@server.com"
                            "subject"
                            "Hello! This is the mail body!"
                             c))))

withServer :: IO a -> IO a
withServer m = withAsync Duta.start (const (threadDelay (1000 * 100) >> m))
