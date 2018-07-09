{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duta
    ( start
    ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as L
import qualified Data.ByteString.Search as S
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.ByteString.Builder as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as Net
import qualified Data.Conduit.Network.TLS as TLS
import           Data.Monoid
import           Data.Typeable

--------------------------------------------------------------------------------
-- Constants

start :: IO ()
start =
  TLS.runTCPServerStartTLS
    (TLS.tlsConfig "*" smtpPort certFile keyFile)
    (\(appData, startTLS) -> do
       putStrLn "Got connection!"
       ok <-
         C.runConduit
           (Net.appSource appData .|
            CL.mapM (\x -> x <$ putStrLn ("<= " <> show x)) .|
            insecureInteraction appData)
       unless ok (putStrLn "no TLS")
       when ok (startTLS secureInteraction)
       threadDelay (1000 * 1000))

insecureInteraction :: (MonadIO m, MonadThrow m) => Net.AppData -> C.ConduitT ByteString c m Bool
insecureInteraction appData = do
  reply appData ServiceReady
  mgreet <- receive (Atto8.choice [Atto8.string "EHLO", Atto8.string "HELO"])
  case mgreet of
    Nothing -> pure False
    Just {} -> do
      liftIO (putStrLn "Received HELO")
      reply appData (Okay " OK")
      mstart <- receive (Atto8.string "STARTTLS")
      case mstart of
        Nothing -> do
          liftIO (putStrLn "wtf")
          pure False
        Just {} -> do
          reply appData ServiceReady
          pure True

secureInteraction :: Net.AppData -> IO ()
secureInteraction appData = do
  putStrLn "Starting TLS ..."
  catch
    (C.runConduit
       (Net.appSource appData .| CL.mapM (\x -> x <$ putStrLn ("<= " <> show x)) .| do
          mgreet <-
            receive (Atto8.choice [Atto8.string "EHLO", Atto8.string "HELO"])
          case mgreet of
            Nothing -> pure ()
            Just {} -> do
              liftIO (putStrLn "Received HELO")
              reply appData (Okay " OK")
              from <- receive (Atto8.string "MAIL FROM:")
              reply appData (Okay " OK")
              to <- receive (Atto8.string "RCPT TO:")
              reply appData (Okay " OK")
              _ <- receive (Atto8.string "DATA")
              reply appData StartMailInput
              data' <- consume dottedParser
              reply appData (Okay " OK")
              _ <- receive (Atto8.string "QUIT")
              reply appData Closing
              liftIO (print ("From", from, "to", to, "data", data'))))
    (\(SomeException e) -> do
       liftIO (putStrLn ("Server exception: " ++ show e ++ " :: " ++ show (typeOf e))))

data FSM = Init | FirstR | FirstN | Dot | SecondR

dottedParser :: Atto8.Parser ByteString
dottedParser = do
  str <-
    Atto8.scan
      Init
      (\fsm c ->
         case fsm of
           Init ->
             pure
               (case c of
                  '\r' -> FirstR
                  _ -> fsm)
           FirstR ->
             pure
               (case c of
                  '\n' -> FirstN
                  _ -> Init)
           FirstN ->
             pure
               (case c of
                  '.' -> Dot
                  _ -> Init)
           Dot ->
             pure
               (case c of
                  '.' -> Init
                  '\r' -> SecondR
                  _ -> Init)
           SecondR ->
             case c of
               '\n' -> Nothing
               _ -> Just Init
           )
  pure
    (L.toStrict
       (S.replace
          "\n.."
          ("\n." :: ByteString)
          (S.take (S8.length str - endingLength) str)))
  where
    endingLength = S.length "\r\n.\r\n"

receive :: (MonadThrow m) => Atto8.Parser b -> C.ConduitM ByteString c m (Maybe b)
receive p =
  fmap
    (fmap snd)
    (CA.conduitParser (p <* Atto8.takeWhile (/= '\n') <* Atto8.char '\n') .|
     C.await)


consume :: MonadThrow m => Atto8.Parser b -> C.ConduitM ByteString c m (Maybe b)
consume p =
  fmap
    (fmap snd)
    (CA.conduitParser p .|
     C.await)

reply :: MonadIO m => Net.AppData -> Reply -> m ()
reply appData rep =
  liftIO
    (C.runConduit
       (C.yield (buildReply rep <> "\n") .| CB.builderToByteString .|
        CL.mapM (\x -> x <$ S8.putStrLn ("=> " <> x)) .|
        Net.appSink appData))

data Reply =
  ServiceReady | Okay !ByteString | StartMailInput | Closing

buildReply :: Reply -> L.Builder
buildReply =
  \case
    ServiceReady -> L.intDec 220
    Closing -> L.intDec 221
    Okay str -> L.intDec 250 <> L.byteString str
    StartMailInput -> "354 Start mail input; end with <CRLF>.<CRLF>"

--------------------------------------------------------------------------------
-- Constants

certFile :: FilePath
certFile = "data/server.crt"

keyFile :: FilePath
keyFile = "data/server.key"

-- | https://www.mailgun.com/blog/25-465-587-what-port-should-i-use
smtpPort :: Int
smtpPort = 587
