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

module Duta.Web.Views where

import           Control.Monad
import           Data.Function
import           Data.Graph
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Database.Persist
import           Duta.Types.Label
import           Duta.Types.Model
import           Duta.Web.Foundation
import           Duta.Web.Types
import           Lucid
import           Text.Links
import           Yesod.Auth

listThreads :: (Route App -> Text) -> [(Entity Thread, [Entity Tag])] -> Html ()
listThreads url labelledThreads = do
  p_ [class_ "main-actions"]
    (do a_ [href_ (url InboxR)] "Inbox"
        a_ [href_ (url AllR)] "All"
        a_ [href_ (url DeletedR)] "Deleted"
        a_ [href_ (url SpamR)] "Spam")
  when (null labelledThreads) (p_ "No messages!")
  mapM_
    (\(Entity threadId thread, labels) -> do
       let unreadClass =
             if elem Unread (map (tagLabel . entityVal) labels)
               then "thread-preview-unread"
               else ""
       div_
         [class_ "thread-preview"]
         (do timestamp_ (threadUpdated thread)
             span_
               [class_ ("messages-count " <> unreadClass)]
               (do "("
                   toHtml (show (threadMessages thread))
                   ")")
             a_
               [ class_ ("thread-preview-link " <> unreadClass)
               , href_ (url (ThreadR threadId))
               ]
               (toHtml (if T.null (threadSubject thread)
                           then "No subject"
                           else threadSubject thread))))
    labelledThreads

viewThread ::
     [(a, Entity Tag)]
  -> Entity Thread
  -> [Tree (Message, Key Message, c)]
  -> [PlainTextPart]
  -> [Attachment]
  -> (Route App -> Text)
  -> Html ()
viewThread labels (Entity threadId thread) forest plainParts _attachments url =
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
             [class_ "thread"]
             (do topnav_ url
                 h1_ (toHtml (threadSubject thread))
                 p_
                   [class_ "thread-actions"]
                   (do let labels' = map (tagLabel . entityVal . snd) labels
                       if elem Inbox labels'
                         then do
                           a_ [href_ (url InboxR)] "Back to Inbox"
                           a_
                             [href_ (url (RemoveLabelR threadId Inbox))]
                             "Archive"
                         else do
                           a_ [href_ (url AllR)] "Back to all"
                           a_
                             [href_ (url (ApplyLabelR threadId Inbox))]
                             "Move to Inbox"
                       unless
                         (elem Unread labels')
                         (a_
                            [href_ (url (ApplyLabelR threadId Unread))]
                            "Mark unread")
                       if elem Muted labels'
                         then a_
                                [href_ (url (RemoveLabelR threadId Muted))]
                                "Unmute"
                         else a_
                                [href_ (url (ApplyLabelR threadId Muted))]
                                "Mute"
                       if elem Deleted labels'
                         then a_
                                [href_ (url (RemoveLabelR threadId Deleted))]
                                "Undelete"
                         else a_
                                [href_ (url (ApplyLabelR threadId Deleted))]
                                "Delete"
                       if elem Spam labels'
                         then a_
                                [href_ (url (RemoveLabelR threadId Spam))]
                                "Not Spam"
                         else a_
                                [href_ (url (ApplyLabelR threadId Spam))]
                                "Spam")
                 void
                   (displayForest
                      displayMessage
                      (fmap (fmap (\(n, k, _) -> Entity k n)) forest)))))
  where
    displayMessage (Entity messageId message) =
      div_
        [class_ "message"]
        (do div_
              [class_ "message-header"]
              (do timestamp_ (messageReceived message)
                  div_
                    [class_ "message-menu"]
                    (a_ [href_ (url (OriginalR messageId))] "View Original")
                  div_
                    [class_ "message-from"]
                    (do strong_ "From: "
                        toHtml (messageFrom message))
                  div_
                    [class_ "message-to"]
                    (do strong_ "To: "
                        toHtml (messageTo message)))
            div_
              [class_ "message-body"]
              (mapM_
                 (\plainTextPart ->
                    div_
                      [class_ "plain-text-part"]
                      (mapM_
                         (\ls ->
                            div_
                              [ class_
                                  (if any (T.isPrefixOf ">") ls
                                     then "text-quote"
                                     else "text-plain")
                              ]
                              (sequence_
                                 (intersperse
                                    (br_ [])
                                    (map
                                       (mconcat .
                                        (map
                                           (either
                                              (\uri ->
                                                 a_
                                                   [ href_ (T.pack (show uri))
                                                   , rel_ "noreferrer"
                                                   ]
                                                   (toHtml (show uri)))
                                              toHtml) .
                                         explodeLinks))
                                       ls))))
                         (groupBy
                            (on (==) (T.isPrefixOf ">"))
                            (T.lines (plainTextPartContent plainTextPart)))))
                 myParts))
      where
        myParts =
          sortBy
            (comparing plainTextPartOrdering)
            (filter ((== messageId) . plainTextPartMessage) plainParts)

displayForest :: (a -> Html ()) -> Forest a -> Html ()

displayForest render xs = mapM_ (displayTree render) xs

displayTree :: (a -> Html ()) -> Tree a -> Html ()
displayTree render (Node parent children) = do
  render parent
  div_ [class_ "message-children"] (displayForest render children)

timestamp_ :: UTCTime -> Html ()
timestamp_ t =
  div_
    [class_ "message-timestamp"]
    (em_ (toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t)))

topnav_ :: (Route App -> Text) -> Html ()
topnav_ url =
  div_
    [class_ "menu"]
    (a_ [class_ "menu-logout", href_ (url (AuthR LogoutR))] "Logout")
