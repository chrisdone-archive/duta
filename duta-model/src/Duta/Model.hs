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
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
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
  insertContent msgId Nothing value

-- | Insert a message part for a given message.
insertContent ::
     MonadIO m
  => Key Message
  -> Maybe (Key MultiPart)
  -> MIME.MIMEValue
  -> ReaderT Persistent.SqlBackend m ()
insertContent msgId mparent value =
  case (MIME.mime_val_content value) of
    MIME.Multi values -> do
      parent <-
        Persistent.insert
          (MultiPart msgId mparent (MIME.showType (MIME.mime_val_type value)))
      mapM_ (insertContent msgId (Just parent)) values
    MIME.Single text ->
      case MIME.mimeType (MIME.mime_val_type value) of
        MIME.Text "plain" ->
          void (Persistent.insert (PlainTextPart msgId mparent text))
        MIME.Text "html" ->
          void (Persistent.insert (HtmlPart msgId mparent text))
        _ ->
          void
            (Persistent.insert
               (BinaryPart
                  msgId
                  mparent
                  (MIME.showType (MIME.mime_val_type value))
                  (T.encodeUtf8 text)))

lookupHeader :: MonadThrow f => Text -> MIME.MIMEValue -> f Text
lookupHeader label value =
  case find (\(MIME.MIMEParam k _) -> k == label) (MIME.mime_val_headers value) of
    Just (MIME.MIMEParam _ v) -> pure v
    Nothing -> throwM (MissingHeader label)
