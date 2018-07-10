{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Duta
import qualified Network.HaskellNet.SMTP as HaskellNet
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
             (HaskellNet.doSMTPPort
                "127.0.0.1"
                (fromIntegral testPort)
                (HaskellNet.sendPlainTextMail
                   "chris@chrisdone.com"
                   "sender@server.com"
                   "subject"
                   "Hello! This is the mail body!"))))

withServer :: IO a -> IO a
withServer m =
  withAsync
    (Duta.start " wibble" testPort)
    (const (threadDelay (1000 * 100) >> m))

testPort :: Int
testPort = 5870
