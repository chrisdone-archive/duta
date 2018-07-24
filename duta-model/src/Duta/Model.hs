{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Duta.Model where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Compat
import           Data.Typeable
import           Database.Persist.Sqlite
import           Duta.Types.MIME
import           Duta.Types.Model
import           System.Time (CalendarTime)
import qualified Text.Parsec.Rfc2822 as Rfc2822

data ModelError = MissingHeader Text
  deriving ( Show, Typeable)
instance Exception ModelError

insertModelMessage ::
     (MonadIO m, MonadLogger m, MonadThrow m)
  => UTCTime
  -> Rfc2822.GenericMessage BodyTree
  -> ReaderT SqlBackend m ()
insertModelMessage received (Rfc2822.Message fields bodyTree) = do
  from <-
    findHeader
      "From"
      (\case
         Rfc2822.From from -> Just (T.intercalate ", " (map nameAddrText from))
         _ -> Nothing)
  to <-
    findHeader
      "To"
      (\case
         Rfc2822.To to -> Just (T.intercalate ", " (map nameAddrText to))
         _ -> Nothing)
  subject <-
    findHeader
      "Subject"
      (\case
         Rfc2822.Subject subject -> Just (T.pack subject)
         _ -> Nothing)
  date <-
    findHeader
      "Date"
      (\case
         Rfc2822.Date date -> Just date
         _ -> Nothing)

  _msgId <-
    insert
      (Message
         { messageReceived = received
         , messageAuthored = toUTCTime date
         , messageFrom = from
         , messageTo = to
         , messageSubject = subject
         })
  pure ()
  where
    findHeader label pred =
      case listToMaybe (mapMaybe pred fields) of
        Just v -> pure v
        Nothing -> do
          logError ("Could not find header: " <> label)
          throwM (MissingHeader label)

nameAddrText :: Rfc2822.NameAddr -> Text
nameAddrText (Rfc2822.NameAddr _mlocal domain) = T.pack domain
