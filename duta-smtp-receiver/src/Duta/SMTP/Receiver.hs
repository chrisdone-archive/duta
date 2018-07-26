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
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           Database.Persist.Sql.Types.Internal

--------------------------------------------------------------------------------
-- Constants

data DutaException
  = ClientQuitUnexpectedly
  | ClientTimeout
  | FailedToParseInput
  deriving (Typeable, Show)
instance Exception DutaException

data Start m = Start
  { startHostname :: String
  , startPort :: Int
  , startOnMessage :: MIMEValue -> m ()
  , startPool :: Pool SqlBackend
  }

start :: Start (ReaderT SqlBackend (LoggingT IO)) -> LoggingT IO ()
start Start {..} =
  withRunInIO
    (\run ->
       Net.runTCPServer
         (Net.serverSettings startPort "*")
         (\appData ->
            run
              (do let ip = T.pack (show (Net.appSockAddr appData))
                  logInfo ("Got connection from " <> ip)
                  catch
                    (do withResource
                          startPool
                          (runReaderT
                             (C.runConduit
                                ((do reason <-
                                       Connector.appSource
                                         (Connector.defaultConnector appData)
                                     case reason of
                                       Connector.Finished ->
                                         throwM ClientQuitUnexpectedly
                                       Connector.ReadWriteTimeout ->
                                         throwM ClientTimeout) .|
                                 CL.mapM
                                   (\x ->
                                      x <$
                                      logDebug (ip <> " <= " <> T.pack (show x))) .|
                                 interaction
                                   Interaction
                                     { interactionHostname = startHostname
                                     , interactionReply =
                                         liftIO . run . makeReply appData
                                     , interactionOnMessage = startOnMessage
                                     })))
                        logInfo ("Done, closing connection: " <> ip))
                    (\case
                       ClientQuitUnexpectedly ->
                         logError (ip <> " quit unexpectedly.")
                       ClientTimeout -> logWarn (ip <> " timed out.")
                       FailedToParseInput ->
                         logError (ip <> " failed to parse input.")))))

makeReply :: (MonadIO m, MonadThrow m, MonadLogger m) => Net.AppData -> Reply -> m ()
makeReply appData rep = do
  reason <-
    C.runConduit
      (C.yield (L.toStrict (L.toLazyByteString (buildReply rep <> "\r\n"))) .|
       CL.mapM
         (\x ->
            x <$
            logDebug
              (T.pack (show (Net.appSockAddr appData)) <> " => " <>
               T.pack (show x))) .|
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
     (MonadThrow m, MonadLogger m)
  => Interaction c m
  -> C.ConduitT ByteString c m ()
interaction Interaction {..} = do
  interactionReply (ServiceReady (S8.pack interactionHostname))
  receive_ "HELO/EHLO" (Atto8.choice [Atto8.string "EHLO", Atto8.string "HELO"])
  interactionReply (Okay " OK")
  from <- receive "MAIL FROM" (Atto8.string "MAIL FROM:")
  interactionReply (Okay " OK")
  to <- receive "RCPT TO" (Atto8.string "RCPT TO:")
  interactionReply (Okay " OK")
  receive_ "DATA" (Atto8.string "DATA")
  interactionReply StartMailInput
  data' <- consume "<CLRF>.<CLRF> terminated data" dottedParser
  interactionReply (Okay " OK")
  receive_ "QUIT" (Atto8.string "QUIT")
  interactionReply Closing
  logInfo
    ("Message from " <> T.decodeUtf8 from <> ", to " <> T.decodeUtf8 to <>
     ", data: " <>
     T.pack (show data'))
  lift (interactionOnMessage (parseMIMEMessage (T.decodeUtf8 data')))

receive_ :: (MonadThrow m,MonadLogger m) => Text -> Atto8.Parser a -> C.ConduitT ByteString c m ()
receive_ l p = receive l p >> pure ()

receive :: (MonadThrow m,MonadLogger m) => Text -> Atto8.Parser a -> C.ConduitT ByteString c m a
receive label p =
  consume label (p <* Atto8.takeWhile (/= '\n') <* Atto8.char '\n')

consume :: (MonadThrow m, MonadLogger m) => Text -> Atto8.Parser a2 -> C.ConduitT ByteString c m a2
consume label p = do
  r <- (CA.conduitParserEither p .| C.await)
  case r of
    Nothing -> do
      logError ("Client quit unexpectedly while waiting for " <> label)
      throwM ClientQuitUnexpectedly
    Just result ->
      case result of
        Left err -> do
          bs <- C.await
          logError
            (label <> ": Failed to parse input: " <> T.pack (show (show err)) <>
             ", input was: " <>
             T.pack (show bs))
          throwM FailedToParseInput
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
