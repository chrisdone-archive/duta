{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duta.Receiver
    ( start
    , interaction
    , Reply (..)
    ) where

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
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
import qualified Data.Conduit.Network as Net
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           System.IO

--------------------------------------------------------------------------------
-- Constants

data DutaException
  = ClientQuitUnexpectedly
  | FailedToParseInput
  deriving (Typeable, Show)
instance Exception DutaException

start :: String -> Int -> LoggingT IO ()
start str port = do
  liftIO (hSetBuffering stdout NoBuffering)
  withRunInIO
    (\run ->
       Net.runTCPServer
         (Net.serverSettings port "*")
         (\appData ->
            run
              (do logInfo
                    ("Got connection from " <>
                     T.pack (show (Net.appSockAddr appData)))
                  C.runConduit
                    (Net.appSource appData .|
                     CL.mapM
                       (\x ->
                          x <$
                          logDebug
                            (T.pack (show (Net.appSockAddr appData)) <> "<= " <>
                             T.pack (show x))) .|
                     interaction
                       str
                       (liftIO . run . makeReply appData)))))

makeReply :: (MonadIO m, MonadLogger m) => Net.AppData -> Reply -> m ()
makeReply appData rep =
  C.runConduit
    (C.yield (L.toStrict (L.toLazyByteString (buildReply rep <> "\r\n"))) .|
     CL.mapM
       (\x ->
          x <$
          logDebug
            (T.pack (show (Net.appSockAddr appData)) <> "=> " <>
             T.pack (show x))) .|
     Net.appSink appData)

interaction :: (MonadThrow m, MonadLogger m) => String -> (Reply -> C.ConduitT ByteString c m ()) -> C.ConduitT ByteString c m ()
interaction str reply = do
  reply (ServiceReady (S8.pack str))
  receive_ "HELO/EHLO" (Atto8.choice [Atto8.string "EHLO", Atto8.string "HELO"])
  reply (Okay " OK")
  from <- receive "MAIL FROM" (Atto8.string "MAIL FROM:")
  reply (Okay " OK")
  to <- receive "RCPT TO" (Atto8.string "RCPT TO:")
  reply (Okay " OK")
  receive_ "DATA" (Atto8.string "DATA")
  reply StartMailInput
  data' <- consume "<CLRF>.<CLRF> terminated data" dottedParser
  reply (Okay " OK")
  receive_ "QUIT" (Atto8.string "QUIT")
  reply Closing
  logInfo
    ("Message from " <> T.decodeUtf8 from <> ", to " <> T.decodeUtf8 to <>
     ", data: " <>
     T.decodeUtf8 data')

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
