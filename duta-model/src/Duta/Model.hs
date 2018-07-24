{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Duta.Model where

import qualified Codec.MIME.Type as MIME
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import           Data.Time
import           Data.Typeable
import qualified Database.Persist.Sqlite as Persistent
import           Duta.Types.Model

data ModelError = MissingHeader Text
  deriving (Show, Typeable)
instance Exception ModelError

insertModelMessage ::
     (MonadIO m, MonadLogger m, MonadThrow m)
  => UTCTime
  -> MIME.MIMEValue
  -> ReaderT Persistent.SqlBackend m ()
insertModelMessage received value = do
  from <- lookupHeader "from" value
  to <- lookupHeader "to" value
  subject <- lookupHeader "subject" value
  msgId <-
    Persistent.insert
      (Message
         { messageReceived = received
         , messageFrom = from
         , messageTo = to
         , messageSubject = subject
         })
  insertContent msgId value
  pure ()

-- | Insert a message part for a given message.
insertContent ::
     MonadIO m
  => Key Message
  -> MIME.MIMEValue
  -> ReaderT Persistent.SqlBackend m ()
insertContent msgId value =
  case (MIME.mime_val_content value) of
    MIME.Multi values -> mapM_ (insertContent msgId) values
    MIME.Single text ->
      case MIME.mimeType (MIME.mime_val_type value) of
        MIME.Text "plain" -> void (Persistent.insert (PlainTextPart msgId text))
        MIME.Text "html" -> void (Persistent.insert (HtmlPart msgId text))
        _ ->
          void
            (Persistent.insert
               (BinaryPart
                  msgId
                  (MIME.showType (MIME.mime_val_type value))
                  (fromMaybe
                     "7BIT"
                     (lookupHeader "content-transfer-encoding" value))
                  text))

lookupHeader :: MonadThrow f => Text -> MIME.MIMEValue -> f Text
lookupHeader label value =
  case find (\(MIME.MIMEParam k _) -> k == label) (MIME.mime_val_headers value) of
    Just (MIME.MIMEParam _ v) -> pure v
    Nothing -> throwM (MissingHeader label)
