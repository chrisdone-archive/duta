{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duta
    ( start
    , interaction
    , Reply (..)
    ) where

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
import           Data.Monoid
import           System.IO

--------------------------------------------------------------------------------
-- Constants

start :: String -> Int -> IO ()
start str port = do
  hSetBuffering stdout NoBuffering
  Net.runTCPServer
    (Net.serverSettings port "*")
    (\appData -> do
       S8.putStrLn "Got connection!"
       C.runConduit
         (Net.appSource appData .|
          CL.mapM (\x -> x <$ S8.putStrLn (S8.pack ("<= " <> show x))) .|
          interaction
            str
            (\rep ->
               liftIO
                 (C.runConduit
                    (C.yield (buildReply rep <> "\r\n") .|
                     CB.builderToByteString .|
                     CL.mapM (\x -> x <$ S8.putStrLn ("=> " <> x)) .|
                     Net.appSink appData)))))

interaction ::
     (MonadIO m, MonadThrow m)
  => String
  -> (Reply -> C.ConduitT ByteString c m ())
  -> C.ConduitT ByteString c m ()
interaction str reply = do
  reply (ServiceReady (S8.pack str))
  mgreet <-
    receive
      (Atto8.choice [Atto8.string "EHLO", Atto8.string "HELO"] Atto8.<?>
       "HELO/EHLO")
  case mgreet of
    Nothing -> pure ()
    Just {} -> do
      liftIO (S8.putStrLn "Received HELO")
      reply (Okay " OK")
      from <- receive (Atto8.string "MAIL FROM:" Atto8.<?> "MAIL FROM:")
      reply (Okay " OK")
      to <- receive (Atto8.string "RCPT TO:" Atto8.<?> "RCPT TO:")
      reply (Okay " OK")
      _ <- receive (Atto8.string "DATA" Atto8.<?> "DATA")
      reply StartMailInput
      data' <- consume dottedParser
      reply (Okay " OK")
      _ <- receive (Atto8.string "QUIT" Atto8.<?> "QUIT")
      reply Closing
      liftIO (print ("From", from, "to", to, "data", data'))

receive :: (MonadThrow m,MonadIO m) => Atto8.Parser b -> C.ConduitM ByteString c m (Maybe b)
receive p = do
  r <-
    (CA.conduitParserEither (p <* Atto8.takeWhile (/= '\n') <* Atto8.char '\n') .|
     C.await)
  case r of
    Nothing -> Nothing <$ liftIO (putStrLn "Client quit unexpectedly.")
    Just result ->
      case result of
        Left err -> do
          bs <- C.await
          Nothing <$
            liftIO
              (S8.putStrLn
                 (S8.pack
                    ("Failed to parse input: " <> show err <> "\nInput was: " <>
                     show bs)))
        Right (_pos, v) -> pure (Just v)

consume :: (MonadIO m,MonadThrow m) => Atto8.Parser b -> C.ConduitM ByteString c m (Maybe b)
consume p = do
  r <-
    (CA.conduitParserEither p .|
     C.await)
  case r of
    Nothing -> Nothing <$ liftIO (putStrLn "Client quit unexpectedly.")
    Just result ->
      case result of
        Left err -> do
          bs <- C.await
          Nothing <$
            liftIO
              (S8.putStrLn
                 (S8.pack
                    ("Failed to parse input: " <> show err <> "\nInput was: " <>
                     show bs)))
        Right (_pos, v) -> pure (Just v)

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
