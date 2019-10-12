{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- |

module Duta.Web.Controllers where

import           Data.Aeson
import           Data.Graph
import           Data.Maybe
import           Data.Text (Text)
import qualified Database.Esqueleto as E
import           Database.Persist.Postgresql as Persistent
import           Duta.Model
import           Duta.Types.Label
import           Duta.Types.Model
import           Duta.Web.Foundation
import           Duta.Web.Types
import           Duta.Web.Views
import           Lucid
import           Yesod hiding (toHtml, Html)
import           Yesod.Lucid

getInboxR :: Handler TypedContent
getInboxR =
  getThreads
    "inbox"
    "Inbox"
    (getThreadsByQuery
       Query
         { queryLimit = 1000
         , queryIncludeLabels = [Inbox]
         , queryExcludeLabels = [Deleted]
         })

getAllR :: Handler TypedContent
getAllR =
  getThreads
    "all"
    "All"
    (getThreadsByQuery
       Query
         { queryIncludeLabels = []
         , queryExcludeLabels = [Deleted]
         , queryLimit = 30
         })

getDeletedR :: Handler TypedContent
getDeletedR =
  getThreads
    "deleted"
    "Deleted"
    (getThreadsByQuery
       Query
         { queryIncludeLabels = [Deleted]
         , queryExcludeLabels = [Spam]
         , queryLimit = 30
         })

getSpamR :: Handler TypedContent
getSpamR =
  getThreads
    "spam"
    "Spam"
    (getThreadsByQuery
       Query
         {queryIncludeLabels = [Spam], queryExcludeLabels = [], queryLimit = 30})

getThreads :: Text -> Text -> (YesodDB App [TaggedThread]) -> Handler TypedContent
getThreads cls title getter =
  selectRep
    (do provideRep
          (do labelledThreads <- runDB getter
              lucid
                (\url ->
                   doctypehtml_
                     (do head_
                           (do meta_ [charset_ "utf-8"]
                               link_
                                 [ rel_ "stylesheet"
                                 , type_ "text/css"
                                 , href_ (url (StaticR css_duta_css))
                                 ])
                         body_
                           (div_
                              [class_ cls]
                              (do topnav_ url
                                  h1_ (toHtml title)
                                  listThreads url labelledThreads)))))
        provideRep
          (do threads <- runDB getter
              pure (toJSON threads)))

getThreadR :: ThreadId -> Handler TypedContent
getThreadR threadId = do
  (mthread, messages, plainParts, attachments, labels) <-
    runDB (getThread threadId)
  let (g, v2n, k2v) =
        graphFromEdges
          (map
             (\(Entity mid m) ->
                ( m
                , mid
                , map
                    entityKey
                    (filter ((== Just mid) . messageParent . entityVal) messages)))
             messages)
      forest =
        fmap
          (fmap v2n)
          (dfs
             g
             (mapMaybe
                (k2v . entityKey)
                (filter (isNothing . messageParent . entityVal) messages)))
  case mthread of
    Nothing -> notFound
    Just thread ->
      selectRep
        (do provideRep
              (lucid
                 (viewThread
                    labels
                    (Entity threadId thread)
                    forest
                    plainParts
                    attachments))
            provideRep
              (pure
                 (toJSON
                    (object
                       [ "id" .= threadId
                       , "plain_parts" .= plainParts
                       , "attachments" .= attachments
                       , "forest" .= renderForest forest
                       ]))))
      where renderForest xs = toJSON (map renderTree xs)
            renderTree (Node (message, key, keys) children) =
              object
                [ "message" .= message
                , "key" .= key
                , "dependencies" .= keys
                , "children" .= renderForest children
                ]

getApplyLabelR :: ThreadId -> Label -> Handler ()
getApplyLabelR tid label = do
  labels <-
    runDB
      (do labels <-
            E.select
              (E.from
                 (\(threadTag, tag) -> do
                    E.where_
                      ((threadTag E.^. ThreadTagThread E.==. E.val tid) E.&&.
                       (threadTag E.^. ThreadTagTag E.==. tag E.^. TagId))
                    pure tag))
          labelThread label tid
          case label of
            Spam -> labelThread Deleted tid
            _ -> pure ()
          pure labels)
  let goBack =
        if elem Inbox (map (tagLabel . entityVal) labels)
          then redirect InboxR
          else redirect AllR
  case label of
    Unread -> goBack
    Deleted -> goBack
    Spam -> goBack
    Muted -> goBack
    _ -> redirect (ThreadR tid)

getRemoveLabelR :: ThreadId -> Label -> Handler ()
getRemoveLabelR tid label = do
  runDB (unlabelThread label tid)
  case label of
    Inbox -> redirect InboxR
    _ -> redirect (ThreadR tid)

getOriginalR :: MessageId -> Handler RepPlain
getOriginalR mid = do
  morig <- runDB (selectFirst [OriginalMessageMessage ==. mid] [])
  case morig of
    Nothing -> notFound
    Just (Entity _ o) -> pure (RepPlain (toContent (originalMessageContents o)))
