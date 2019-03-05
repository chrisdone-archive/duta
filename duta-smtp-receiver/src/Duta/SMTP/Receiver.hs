{-# LANGUAGE ScopedTypeVariables #-}
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
    , FromTo (..)
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
import           Data.Char
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as Net hiding (appSource, appSink)
import qualified Data.Conduit.Network.Timeout as Connector
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Database.Persist.Sql.Types.Internal
import           System.IO

--------------------------------------------------------------------------------
-- Constants

data DutaException
  = ClientQuitUnexpectedly Text
  | ClientTimeout
  | FailedToParseInput CA.ParseError (Maybe ByteString)
  deriving (Typeable, Show)
instance Exception DutaException

data Start m = Start
  { startHostname :: String
  , startPort :: Int
  , startOnMessage :: FromTo -> ByteString -> MIMEValue -> m ()
  , startPool :: Pool SqlBackend
  }

start :: Start (LoggingT IO) -> LoggingT IO ()
start Start {..} = do
  UnliftIO run <- askUnliftIO
  liftIO
    (do hSetBuffering stdout NoBuffering
        Net.runTCPServer (Net.serverSettings startPort "*") (run . handler run))
  where
    handler run appData =
      catch
        (do logInfo (wrap "CONNECT")
            catch
              (catch
                 (do conduit
                     logInfo (wrap "SUCCESS"))
                 (\(x :: IOError) ->
                    logError (wrap ("IOError: " <> T.pack (show x)))))
              (\(x :: IOException) ->
                 logError (wrap ("IOException: " <> T.pack (show x)))))
        (\case
           ClientQuitUnexpectedly q -> logError (wrap ("QUIT UNEXPECTEDLY: " <> q))
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
                Connector.Finished -> throwM (ClientQuitUnexpectedly "Finished")
                Connector.ReadWriteTimeout -> throwM ClientTimeout
            logger =
              CL.mapM
                (\x -> do
                   -- liftIO (S.appendFile (fp appData) x)
                   x <$
                     logDebug
                       (wrap "IN: " <>
                        (T.pack (take 30 (show x)) <> " (" <>
                         T.pack (show (S.length x)) <> " bytes)")))
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
         (\x -> do
            -- liftIO (S.appendFile (fp appData) x)
            x <$
             logDebug
               (T.pack (show (Net.appSockAddr appData)) <> ": OUT: " <>
                T.pack (show x))) .|
       Connector.appSink (Connector.defaultConnector appData))
  case reason of
    Connector.ReadWriteTimeout -> throwM ClientTimeout
    Connector.Finished -> pure ()

-- fp :: Net.AppData -> FilePath
-- fp appData =
--   "transcript-" ++
--   (map (\c -> if isAlphaNum c || c == '.'
--                  then c
--                  else '_') (show (Net.appSockAddr appData))) ++
--   ".txt"

data FromTo = FromTo ByteString ByteString

data Interaction c m = Interaction
  { interactionHostname :: String
  , interactionOnMessage :: FromTo -> ByteString -> MIMEValue -> m ()
  , interactionReply :: Reply -> C.ConduitT ByteString c m ()
  }

interaction ::
     (MonadThrow m, MonadLogger m)
  => Interaction c m
  -> C.ConduitT ByteString c m ()
interaction Interaction {..} = do
  interactionReply (ServiceReady (S8.pack interactionHostname))
  receive_ "HELO/EHLO" (Atto8.choice [ciByteString "EHLO", ciByteString "HELO"])
  interactionReply (Okay " OK")
  from <- receive "MAIL FROM" (ciByteString "MAIL FROM:")
  interactionReply (Okay " OK")
  to <- receive "RCPT TO" (ciByteString "RCPT TO:")
  interactionReply (Okay " OK")
  receive_ "DATA" (ciByteString "DATA")
  interactionReply StartMailInput
  data' <- consume "dottedParser" dottedParser
  lift (interactionOnMessage (FromTo from to) data' (parseMIMEMessage (T.pack (S8.unpack data'))))
  interactionReply (Okay " OK")
  receive_ "QUIT" (ciByteString "QUIT")
  interactionReply Closing

-- https://tools.ietf.org/html/rfc5321
-- Although EHLO keywords may be specified in upper, lower, or mixed
-- case, they MUST always be recognized and processed in a case-
-- insensitive manner.  This is simply an extension of practices
-- specified in RFC 821 and Section 2.4.
ciByteString :: ByteString -> Atto8.Parser ByteString
ciByteString i0 =
  Atto8.scan
    i0
    (\i w8 -> do
       (c, cs) <- S8.uncons i
       guard (toUpper c == toUpper w8)
       pure cs)
{-# INLINE ciByteString #-}

receive_ :: MonadThrow m => Text -> Atto8.Parser a -> C.ConduitT ByteString c m ()
receive_ l p = receive l p >> pure ()

receive :: MonadThrow m => Text -> Atto8.Parser a2 -> C.ConduitT ByteString c m a2
receive l p =
  consume l (p <* Atto8.takeWhile (/= '\n') <* Atto8.char '\n')

consume :: (MonadThrow m) => Text -> Atto8.Parser a2 -> C.ConduitT ByteString c m a2
consume l p = do
  r <- (CA.conduitParserEither p .| C.await)
  case r of
    Nothing -> do
      throwM (ClientQuitUnexpectedly l)
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
