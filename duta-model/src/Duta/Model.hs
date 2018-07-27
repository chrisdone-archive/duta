{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Duta.Model where

import qualified Codec.MIME.Type as MIME
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Typeable
import           Database.Persist ((<-.), (=.), (+=.))
import qualified Database.Persist.Sqlite as Persistent
import           Duta.Types.Model
import           Duta.Types.Order

data ModelError = MissingHeader Text
  deriving (Show, Typeable)
instance Exception ModelError

-- | Insert a message, mapping it to any existing thread.
insertModelMessage ::
     (MonadIO m, MonadLogger m, MonadThrow m)
  => UTCTime
  -> MIME.MIMEValue
  -> ReaderT Persistent.SqlBackend m ()
insertModelMessage received value = do
  from <- lookupHeader "from" value
  to <- lookupHeader "to" value
  subject <- lookupHeader "subject" value
  (threadId, mparentId) <- getThreadId subject value
  msgId <-
    Persistent.insert
      (Message
         { messageReceived = received
         , messageFrom = from
         , messageTo = to
         , messageSubject = subject
         , messageThread = threadId
         , messageParent = mparentId
         , messageIdentifier = lookupHeader "message-id" value
         })
  now <- liftIO getCurrentTime
  Persistent.update threadId [ThreadUpdated =. now, ThreadMessages +=. 1]
  evalStateT (insertContent msgId Nothing value) (Order 0)

-- | Figure out the thread that a message belongs to.
getThreadId ::
     MonadIO m
  => Text
  -> MIME.MIMEValue
  -> ReaderT Persistent.SqlBackend m (ThreadId, Maybe MessageId)
getThreadId subject value =
  case lookupHeader "references" value of
    Nothing -> newThread
    Just ref -> do
      let messageIdsParentFirst =
            maybe [] pure (lookupHeader "in-reply-to" value) ++
            reverse (T.words ref)
      matching <-
        Persistent.selectList
          [MessageIdentifier <-. map Just messageIdsParentFirst]
          []
      let mparent =
            listToMaybe
              (mapMaybe
                 (\ident ->
                    find
                      ((== Just ident) .
                       messageIdentifier . Persistent.entityVal)
                      matching)
                 messageIdsParentFirst)
      case mparent of
        Just (Persistent.Entity messageId m) ->
          pure (messageThread m, Just messageId)
        Nothing -> newThread
  where
    newThread = do
      now <- liftIO getCurrentTime
      threadId <-
        Persistent.insert
          (Thread
             { threadSubject = subject
             , threadCreated = now
             , threadUpdated = now
             , threadArchived = False
             , threadMessages = 0
             })
      pure (threadId, Nothing)

-- | Insert a message part for a given message.
insertContent ::
     MonadIO m
  => Key Message
  -> Maybe (Key MultiPart)
  -> MIME.MIMEValue
  -> StateT Order (ReaderT Persistent.SqlBackend m) ()
insertContent msgId mparent value =
  case (MIME.mime_val_content value) of
    MIME.Multi values -> do
      ordering <- get
      parent <-
        lift
          (Persistent.insert
             (MultiPart
                { multiPartOrdering = ordering
                , multiPartMessage = msgId
                , multiPartParent = mparent
                , multiPartContentType =
                    (MIME.showType (MIME.mime_val_type value))
                }))
      mapM_ (insertContent msgId (Just parent)) values
    MIME.Single text -> do
      ordering <- getOrder
      lift
        (case MIME.mimeType (MIME.mime_val_type value) of
           MIME.Text "plain" ->
             void
               (Persistent.insert
                  (PlainTextPart
                     { plainTextPartOrdering = ordering
                     , plainTextPartMessage = msgId
                     , plainTextPartParent = mparent
                     , plainTextPartContent = text
                     }))
           MIME.Text "html" ->
             void
               (Persistent.insert
                  (HtmlPart
                     { htmlPartOrdering = ordering
                     , htmlPartMessage = msgId
                     , htmlPartParent = mparent
                     , htmlPartContent = text
                     }))
           _ -> do
             void
               (Persistent.insert
                  (BinaryPart
                     { binaryPartOrdering = ordering
                     , binaryPartMessage = msgId
                     , binaryPartParent = mparent
                     , binaryPartContentType =
                         MIME.showType (MIME.mime_val_type value)
                     , binaryPartContent = T.encodeUtf8 text
                     })))
  where
    getOrder = get <* modify incOrder

lookupHeader :: MonadThrow f => Text -> MIME.MIMEValue -> f Text
lookupHeader label value =
  case find (\(MIME.MIMEParam k _) -> k == label) (MIME.mime_val_headers value) of
    Just (MIME.MIMEParam _ v) -> pure v
    Nothing -> throwM (MissingHeader label)
