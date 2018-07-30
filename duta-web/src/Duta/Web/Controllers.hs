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

import           Data.Graph
import           Data.Maybe
import qualified Database.Esqueleto as E
import           Database.Persist.Postgresql as Persistent
import           Duta.Model
import           Duta.Types.Label
import           Duta.Types.Model
import           Duta.Web.Foundation
import           Duta.Web.Views
import           Lucid
import           Yesod hiding (toHtml, Html)
import           Yesod.Auth
import           Yesod.Lucid

getInboxR :: Handler LucidHtml
getInboxR = do
  mu <- maybeAuthId
  case mu of
    Nothing -> lucid (\url -> p_ (a_ [href_ (url (AuthR LoginR))] "Login"))
    Just {} -> do
      labelledThreads <-
        runDB
          (getThreadsByQuery
             Query
               {queryLimit = 30, queryIncludeLabels = [Inbox], queryExcludeLabels = [Deleted]})
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
                      [class_ "inbox"]
                      (do topnav_ url
                          h1_ "Inbox"
                          listThreads url labelledThreads))))

getAllR :: Handler LucidHtml
getAllR = do
  mu <- maybeAuthId
  case mu of
    Nothing -> lucid (\url -> p_ (a_ [href_ (url (AuthR LoginR))] "Login"))
    Just {} -> do
      labelledThreads <-
        runDB
          (getThreadsByQuery
             Query
               { queryIncludeLabels = []
               , queryExcludeLabels = [Deleted]
               , queryLimit = 30
               })
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
                      [class_ "all"]
                      (do topnav_ url
                          h1_ "All"
                          listThreads url labelledThreads))))

getDeletedR :: Handler LucidHtml
getDeletedR = do
  mu <- maybeAuthId
  case mu of
    Nothing -> lucid (\url -> p_ (a_ [href_ (url (AuthR LoginR))] "Login"))
    Just {} -> do
      labelledThreads <-
        runDB
          (getThreadsByQuery
             Query
               { queryIncludeLabels = [Deleted]
               , queryExcludeLabels = [Spam]
               , queryLimit = 30
               })
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
                      [class_ "deleted"]
                      (do topnav_ url
                          h1_ "Deleted"
                          listThreads url labelledThreads))))

getSpamR :: Handler LucidHtml
getSpamR = do
  mu <- maybeAuthId
  case mu of
    Nothing -> lucid (\url -> p_ (a_ [href_ (url (AuthR LoginR))] "Login"))
    Just {} -> do
      labelledThreads <-
        runDB
          (getThreadsByQuery
             Query
               { queryIncludeLabels = [Spam]
               , queryExcludeLabels = []
               , queryLimit = 30
               })
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
                      [class_ "spam"]
                      (do topnav_ url
                          h1_ "Spam"
                          listThreads url labelledThreads))))

getThreadR :: ThreadId -> Handler LucidHtml
getThreadR threadId = do
  (mthread, messages, plainParts, labels) <- runDB (getThread threadId)
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
      lucid
        (viewThread labels (Entity threadId thread) forest plainParts)

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
