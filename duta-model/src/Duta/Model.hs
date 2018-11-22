{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Duta.Model where

import qualified Codec.MIME.Type as MIME
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Generics
import           Data.Int
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Data.Time
import qualified Database.Esqueleto as E
import           Database.Persist (Entity, (+=.), (<-.), (=.), (==.), Entity(..))
import qualified Database.Persist.Sqlite as Persistent
import           Duta.Types.Label
import           Duta.Types.Model
import           Duta.Types.Order
import           Text.HTML.DOM
import           Text.XML

data ModelError = MissingHeader Text
  deriving (Show, Typeable)
instance Exception ModelError

-- | Insert a message, mapping it to any existing thread.
insertModelMessage ::
     (MonadIO m, MonadLogger m, MonadThrow m)
  => ByteString
  -> ByteString
  -> UTCTime
  -> MIME.MIMEValue
  -> ByteString
  -> ReaderT Persistent.SqlBackend m ()
insertModelMessage from0 to0 received value original = do
  -- TODO: Handle error case of decodeUf8.
  let from = fromMaybe (T.decodeUtf8 from0) (lookupHeader "from" value)
  let to = fromMaybe (T.decodeUtf8 to0) (lookupHeader "to" value)
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
  labels <-
    fmap
      (map (tagLabel . Persistent.entityVal))
      (E.select
         (E.from
            (\(threadTag, tag) -> do
               E.where_
                 ((threadTag E.^. ThreadTagThread E.==. E.val threadId) E.&&.
                  (threadTag E.^. ThreadTagTag E.==. tag E.^. TagId))
               pure tag)))
  labelThread Unread threadId
  unless (elem Muted labels) (labelThread Inbox threadId)
  _ <- Persistent.insert (OriginalMessage msgId original)
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
             , threadMessages = 0
             })
      pure (threadId, Nothing)

-- | Insert a message part for a given message.
insertContent ::
     (MonadIO m, MonadLogger m)
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
        (do let binaryPart =
                  void
                    (Persistent.insert
                       (BinaryPart
                          { binaryPartOrdering = ordering
                          , binaryPartMessage = msgId
                          , binaryPartParent = mparent
                          , binaryPartAttachment = mimeValueIsAttachment value
                          , binaryPartName = mimeValueFileName value
                          , binaryPartContentType =
                              MIME.showType (MIME.mime_val_type value)
                          , binaryPartContent = T.encodeUtf8 text
                          }))
            if mimeValueIsAttachment value
              then binaryPart
              else case MIME.mimeType (MIME.mime_val_type value) of
                     MIME.Text "plain" -> do
                       text' <-
                         case T.decodeUtf8' (S8.pack (T.unpack text)) of
                           Left e -> do
                             logError
                               ("Unable to parse string: " <> T.pack (show e) <>
                                ", string was: " <>
                                T.pack (show text))
                             pure text
                           Right t -> pure t
                       void
                         (Persistent.insert
                            (PlainTextPart
                               { plainTextPartOrdering = ordering
                               , plainTextPartMessage = msgId
                               , plainTextPartParent = mparent
                               , plainTextPartContent = text'
                               }))
                     MIME.Text "html" -> do
                       text' <-
                         case T.decodeUtf8' (S8.pack (T.unpack text)) of
                           Left e -> do
                             logError
                               ("Unable to parse string: " <> T.pack (show e) <>
                                ", string was: " <>
                                T.pack (show text))
                             pure text
                           Right t -> pure t
                       void
                         (Persistent.insert
                            (HtmlPart
                               { htmlPartOrdering = ordering
                               , htmlPartMessage = msgId
                               , htmlPartParent = mparent
                               , htmlPartContent = text'
                               }))
                     _ -> binaryPart)
  where
    getOrder = get <* modify incOrder

-- | Is the mime value an attachment?
mimeValueIsAttachment :: MIME.MIMEValue -> Bool
mimeValueIsAttachment =
  maybe False ((== MIME.DispAttachment) . MIME.dispType) . MIME.mime_val_disp

-- | Lookup a filename, assuming it's an attachment.
mimeValueFileName :: MIME.MIMEValue -> Maybe Text
mimeValueFileName value = viaContentType <|> viaDisposition
  where
    viaContentType =
      fmap
        MIME.paramValue
        (find
           ((== "name") . MIME.paramName)
           (MIME.mimeParams (MIME.mime_val_type value)))
    viaDisposition = do
      disp <- MIME.mime_val_disp value
      let params = MIME.dispParams disp
      listToMaybe
        (mapMaybe
           (\case
              MIME.Filename fp -> Just fp
              _ -> Nothing)
           params)

lookupHeader :: MonadThrow f => Text -> MIME.MIMEValue -> f Text
lookupHeader label value =
  case find (\(MIME.MIMEParam k _) -> k == label) (MIME.mime_val_headers value) of
    Just (MIME.MIMEParam _ v) -> pure v
    Nothing -> throwM (MissingHeader label)

labelThread :: MonadIO m => Label -> ThreadId -> ReaderT Persistent.SqlBackend m ()
labelThread label threadId = do
  tagId <- getLabelTagId label
  void (Persistent.deleteWhere [ThreadTagThread ==. threadId, ThreadTagTag ==. tagId])
  void (Persistent.insert (ThreadTag threadId tagId))

unlabelThread ::
     MonadIO m => Label -> ThreadId -> ReaderT Persistent.SqlBackend m ()
unlabelThread label threadId = do
  tagId <- getLabelTagId label
  void (Persistent.deleteWhere [ThreadTagThread ==. threadId, ThreadTagTag ==. tagId])

getLabelTagId :: MonadIO m => Label -> ReaderT Persistent.SqlBackend m TagId
getLabelTagId l = do
  mtag <- Persistent.selectFirst [TagLabel ==. l] []
  case mtag of
    Just (Persistent.Entity t _) -> pure t
    Nothing -> newTagFromLabel l

newTagFromLabel :: MonadIO m =>  Label -> ReaderT Persistent.SqlBackend m TagId
newTagFromLabel l = do
  now <- liftIO getCurrentTime
  Persistent.insert
    (Tag {tagLabel = l, tagTitle = labelTitle l, tagApplied = now})

data Query = Query
  { queryIncludeLabels :: [Label]
  , queryExcludeLabels :: [Label]
  , queryLimit :: Int64
  }

-- | Return all threads given by a query.
getThreadsByQuery ::
     MonadIO m
  => Query
  -> ReaderT Persistent.SqlBackend m [(Entity Thread, [Entity Tag])]
getThreadsByQuery query = do
  threads <-
    E.select
      (E.from
         (\thread -> do
            E.where_
              ((case queryExcludeLabels query of
                  [] -> E.val True
                  labels ->
                    thread E.^. ThreadId `E.notIn`
                    E.subList_select
                      (E.from
                         (\(threadTag, tag) -> do
                            E.where_
                              ((threadTag E.^. ThreadTagTag E.==. tag E.^. TagId) E.&&.
                               (tag E.^. TagLabel `E.in_` E.valList labels))
                            pure (threadTag E.^. ThreadTagThread)))) E.&&.
               (case queryIncludeLabels query of
                  [] -> E.val True
                  labels ->
                    thread E.^. ThreadId `E.in_`
                    E.subList_select
                      (E.from
                         (\(threadTag, tag) -> do
                            E.where_
                              ((threadTag E.^. ThreadTagTag E.==. tag E.^. TagId) E.&&.
                               (tag E.^. TagLabel `E.in_` E.valList labels))
                            pure (threadTag E.^. ThreadTagThread)))))
            E.orderBy [E.desc (thread E.^. ThreadUpdated)]
            E.limit (queryLimit query)
            pure thread))
  threadsLabels <-
    E.select
      (E.from
         (\(threadTag, tag) -> do
            E.where_
              ((threadTag E.^. ThreadTagThread `E.in_`
                E.valList (map entityKey threads)) E.&&.
               (threadTag E.^. ThreadTagTag E.==. tag E.^. TagId))
            pure (threadTag, tag)))
  pure
    (map
       (\thread ->
          ( thread
          , mapMaybe
              (\(Entity _ threadTag, label) -> do
                 guard (threadTagThread threadTag == entityKey thread)
                 pure label)
              threadsLabels))
       threads)

getThread ::
     MonadIO m
  => ThreadId
  -> ReaderT Persistent.SqlBackend m ( Maybe Thread
                                     , [Entity Message]
                                     , [PlainTextPart]
                                     , [Attachment]
                                     , [(Entity ThreadTag, Entity Tag)])
getThread threadId = do
  labels <-
    E.select
      (E.from
         (\(threadTag, tag) -> do
            E.where_
              ((threadTag E.^. ThreadTagThread E.==. E.val threadId) E.&&.
               (threadTag E.^. ThreadTagTag E.==. tag E.^. TagId))
            pure (threadTag, tag)))
  mapM_
    Persistent.delete
    (mapMaybe
       (\(threadTag, tag) -> do
          guard (tagLabel (entityVal tag) == Unread)
          pure (entityKey threadTag))
       labels)
  mthread <- Persistent.get threadId
  messages <-
    Persistent.selectList
      [MessageThread ==. threadId]
      [Persistent.Asc MessageReceived]
  plainParts0 <-
    fmap
      (map entityVal)
      (Persistent.selectList
         [PlainTextPartMessage <-. map entityKey messages]
         [])
  plainParts <-
    if null plainParts0
      then fmap
             (map (toPlainTextPart . entityVal))
             (Persistent.selectList
                [HtmlPartMessage <-. map entityKey messages]
                [])
      else pure plainParts0
  binaryParts <-
    E.select
      (E.from
         (\binaryPart -> do
            E.where_
              (binaryPart E.^. BinaryPartMessage `E.in_`
               E.valList (map entityKey messages))
            pure
              ( binaryPart E.^. BinaryPartId
              , binaryPart E.^. BinaryPartMessage
              , binaryPart E.^. BinaryPartName
              , binaryPart E.^. BinaryPartContentType)))
  pure
    ( mthread
    , messages
    , plainParts
    , map
        (\((E.Value b), (E.Value m), (E.Value n), (E.Value c)) ->
           Attachment b m n c)
        binaryParts
    , labels)

toPlainTextPart :: HtmlPart -> PlainTextPart
toPlainTextPart htmlPart =
  PlainTextPart
    { plainTextPartOrdering = htmlPartOrdering htmlPart
    , plainTextPartMessage = htmlPartMessage htmlPart
    , plainTextPartParent = htmlPartParent htmlPart
    , plainTextPartContent =
        T.unlines
          (stripBlankLines
             (dropWhile
                T.null
                (map
                   T.strip
                   (T.lines
                      (T.concat
                         (mapMaybe
                            (\case
                               NodeElement (Element {elementName = Name {nameLocalName = "p"}}) ->
                                 Just "\n"
                               NodeElement (Element {elementName = Name {nameLocalName = "br"}}) ->
                                 Just "\n"
                               NodeElement (Element { elementName = Name {nameLocalName = "a"}
                                                    , elementAttributes = attrs
                                                    }) ->
                                 fmap
                                   (\link -> "<" <> link <> ">")
                                   (M.lookup "href" attrs)
                               NodeContent t -> Just t
                               _ -> Nothing)
                            (listify
                               (const True)
                               (everywhere
                                  (mkT
                                     (\case
                                        NodeElement (Element {elementName = Name {nameLocalName = "style"}}) ->
                                          NodeComment "Skipped style."
                                        NodeElement (Element {elementName = Name {nameLocalName = "script"}}) ->
                                          NodeComment "Skipped script."
                                        e -> e))
                                  (parseLT
                                     (LT.fromStrict (htmlPartContent htmlPart)))))))))))
    }
  where
    stripBlankLines (x:y:xs) =
      if T.null x && T.null y
        then stripBlankLines (y : xs)
        else x : stripBlankLines (y : xs)
    stripBlankLines x = x
