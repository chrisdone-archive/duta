{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duta.SMTP.Receiver
    ( start
    , Start(..)
    , interaction
    , Interaction(..)
    , Reply (..)
    ) where

import           Codec.MIME.Parse
import           Codec.MIME.Type
import           Control.Exception hiding (catch)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
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
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as Net hiding (appSource, appSink)
import qualified Data.Conduit.Network.Timeout as Connector
import           Data.Monoid
import           Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           Database.Persist.Sql.Types.Internal
import           System.IO

--------------------------------------------------------------------------------
-- Constants

data DutaException
  = ClientQuitUnexpectedly
  | ClientTimeout
  | FailedToParseInput CA.ParseError (Maybe ByteString)
  deriving (Typeable, Show)
instance Exception DutaException

data Start m = Start
  { startHostname :: String
  , startPort :: Int
  , startOnMessage :: MIMEValue -> m ()
  , startPool :: Pool SqlBackend
  }

start :: Start (ReaderT SqlBackend (LoggingT IO)) -> LoggingT IO ()
start Start {..} = do
  UnliftIO run <- askUnliftIO
  liftIO
    (do hSetBuffering stdout NoBuffering
        Net.runTCPServer (Net.serverSettings startPort "*") (run . handler run))
  where
    handler run appData =
      catch
        (do logInfo (wrap "CONNECT")
            withResource startPool (runReaderT conduit)
            logInfo (wrap "SUCCESS"))
        (\case
           ClientQuitUnexpectedly -> logError (wrap "QUIT UNEXPECTEDLY")
           ClientTimeout -> logWarn (wrap "TIMED OUT")
           FailedToParseInput e inp ->
             logError
               (wrap
                  ("PARSE ERROR: " <> T.pack (show e) <> "\n" <>
                   T.pack (show inp))))
      where
        conduit = C.runConduit (source .| logger .| sink)
          where
            source = do
              reason <- Connector.appSource (Connector.defaultConnector appData)
              case reason of
                Connector.Finished -> throwM ClientQuitUnexpectedly
                Connector.ReadWriteTimeout -> throwM ClientTimeout
            logger =
              CL.mapM (\x -> x <$ logDebug (wrap "IN: " <> T.pack (show x)))
            sink =
              interaction
                Interaction
                  { interactionHostname = startHostname
                  , interactionReply = liftIO . run . makeReply appData
                  , interactionOnMessage = startOnMessage
                  }
        wrap s = T.pack (show (Net.appSockAddr appData)) <> ": " <> T.take 60 s

makeReply :: (MonadIO m, MonadThrow m, MonadLogger m) => Net.AppData -> Reply -> m ()
makeReply appData rep = do
  reason <-
    C.runConduit
      (C.yield (L.toStrict (L.toLazyByteString (buildReply rep <> "\r\n"))) .|
       CL.mapM
         (\x ->
            x <$
            logDebug
              (T.pack (show (Net.appSockAddr appData)) <> ": OUT: " <>
               T.pack (take 60 (show x)))) .|
       Connector.appSink (Connector.defaultConnector appData))
  case reason of
    Connector.ReadWriteTimeout -> throwM ClientTimeout
    Connector.Finished -> pure ()

data Interaction c m = Interaction
  { interactionHostname :: String
  , interactionOnMessage :: MIMEValue -> m ()
  , interactionReply :: Reply -> C.ConduitT ByteString c m ()
  }

interaction ::
     (MonadThrow m)
  => Interaction c m
  -> C.ConduitT ByteString c m ()
interaction Interaction {..} = do
  interactionReply (ServiceReady (S8.pack interactionHostname))
  receive_ (Atto8.choice [Atto8.string "EHLO", Atto8.string "HELO"])
  interactionReply (Okay " OK")
  _from <- receive (Atto8.string "MAIL FROM:")
  interactionReply (Okay " OK")
  _to <- receive (Atto8.string "RCPT TO:")
  interactionReply (Okay " OK")
  receive_ (Atto8.string "DATA")
  interactionReply StartMailInput
  data' <- consume dottedParser
  interactionReply (Okay " OK")
  receive_ (Atto8.string "QUIT")
  interactionReply Closing
  lift (interactionOnMessage (parseMIMEMessage (T.decodeUtf8 data')))

receive_ :: (MonadThrow m) => Atto8.Parser a -> C.ConduitT ByteString c m ()
receive_ p = receive p >> pure ()

receive :: (MonadThrow m) => Atto8.Parser a -> C.ConduitT ByteString c m a
receive p =
  consume (p <* Atto8.takeWhile (/= '\n') <* Atto8.char '\n')

consume :: (MonadThrow m) => Atto8.Parser a2 -> C.ConduitT ByteString c m a2
consume p = do
  r <- (CA.conduitParserEither p .| C.await)
  case r of
    Nothing -> do
      throwM (ClientQuitUnexpectedly)
    Just result ->
      case result of
        Left err -> do
          bs <- C.await
          throwM (FailedToParseInput err bs)
        Right (_pos, v) -> pure v

data Reply
  = ServiceReady !ByteString
  | Okay !ByteString
  | StartMailInput
  | Closing
  deriving (Show, Eq)

buildReply :: Reply -> L.Builder
buildReply =
  \case
    ServiceReady str -> L.intDec 220 <> L.byteString str
    Closing -> L.intDec 221 <> " OK"
    Okay str -> L.intDec 250 <> L.byteString str
    StartMailInput -> "354 Start mail input; end with <CRLF>.<CRLF>"

data FSM = Init | FirstR | FirstN | Dot | SecondR

dottedParser :: Atto8.Parser ByteString
dottedParser = do
  str <- Atto8.scan Init machine
  _ <- Atto8.char '\n'
  pure
    (L.toStrict
       (S.replace
          "\n.."
          ("\n." :: ByteString)
          (S.take (S8.length str - endingLength) str)))
  where
    endingLength = S.length "\r\n.\r"
    machine fsm c =
      case fsm of
        FirstR
          | c == '\n' -> pure FirstN
        FirstN
          | c == '.' -> pure Dot
        Dot
          | c == '\r' -> pure SecondR
        SecondR
          | c == '\n' -> Nothing
        _ | c == '\r' -> pure FirstR
          | otherwise -> pure Init
