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

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Function
import           Data.Generics (listify)
import           Data.Graph
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Time
import qualified Database.Esqueleto as E
import           Database.Persist.Postgresql as Persistent
import           Development
import           Duta.Model
import           Duta.Types.Label
import           Duta.Types.Model
import           Lucid
import           Options.Applicative.Simple
import           System.Environment
import           Text.HTML.DOM
import           Text.XML
import           Yesod hiding (toHtml, Html)
import           Yesod.Auth
import           Yesod.Auth.Hardcoded
import           Yesod.Auth.Message (AuthMessage(InvalidLogin))
import           Yesod.EmbeddedStatic
import           Yesod.Lucid

--------------------------------------------------------------------------------
-- App

data SiteManager = SiteManager
  { manUserName :: Text
  , manPassWord :: Text
  } deriving (Show, Eq)

data App = App
  { appPool :: Pool SqlBackend
  , appRoot :: Text
  , appSiteManager :: SiteManager
  , appStatic :: EmbeddedStatic
  }

mkEmbeddedStatic development "embeddedStatic" [embedDir "static"]

mkYesod "App" [parseRoutes|
  /static StaticR EmbeddedStatic appStatic
  / InboxR GET
  /all AllR GET
  /auth AuthR Auth getAuth
  /thread/#ThreadId ThreadR GET
  /apply-label/#ThreadId/#Label ApplyLabelR GET
  /remove-label/#ThreadId/#Label RemoveLabelR GET
|]

instance Yesod App where
  addStaticContent = embedStaticContent appStatic StaticR Right
  approot = ApprootMaster appRoot
  maximumContentLength _ _ = Just (1024 * 20)
  authRoute _ = Just $ AuthR LoginR
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized _ _ = do
    mu <- maybeAuthId
    return (maybe AuthenticationRequired (const Authorized) mu)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB act = do
    App {appPool = pool} <- getYesod
    runSqlPool act pool

instance YesodAuth App where
  type AuthId App = Text
  loginDest _ = InboxR
  logoutDest _ = AuthR LoginR
  authPlugins _ = [authHardcoded]
  authenticate Creds {..} = do
    app <- getYesod
    pure
      (case credsPlugin of
         "hardcoded" ->
           case credsIdent == manUserName (appSiteManager app) of
             True -> Authenticated credsIdent
             False -> UserError InvalidLogin
         _ -> ServerError "Invalid auth plugin.")

instance YesodAuthHardcoded App where
  validatePassword u p = do
    siteManager <- fmap appSiteManager getYesod
    pure (SiteManager u p == siteManager)
  doesUserNameExist u = do
    siteManager <- fmap appSiteManager getYesod
    pure (u == manUserName siteManager)

instance YesodAuthPersist App where
  type AuthEntity App = SiteManager
  getAuthEntity u = do
    siteManager <- fmap appSiteManager getYesod
    if manUserName siteManager == u
      then pure (Just siteManager)
      else pure Nothing

--------------------------------------------------------------------------------
-- Inbox page

getInboxR :: Handler LucidHtml
getInboxR = do
  mu <- maybeAuthId
  case mu of
    Nothing -> lucid (\url -> p_ (a_ [href_ (url (AuthR LoginR))] "Login"))
    Just {} -> do
      labelledThreads <-
        runDB
          (do threads <-
                E.select
                  (E.from
                     (\(thread, threadTag, tag) -> do
                        E.where_
                          ((threadTag E.^. ThreadTagTag E.==. tag E.^. TagId) E.&&.
                           (threadTag E.^. ThreadTagThread E.==. thread E.^.
                            ThreadId) E.&&.
                           (tag E.^. TagLabel E.==. E.val Inbox))
                        E.orderBy [E.desc (thread E.^. ThreadUpdated)]
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
                             guard
                               (threadTagThread threadTag == entityKey thread)
                             pure label)
                          threadsLabels))
                   threads))
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

--------------------------------------------------------------------------------
-- All page

getAllR :: Handler LucidHtml
getAllR = do
  mu <- maybeAuthId
  case mu of
    Nothing -> lucid (\url -> p_ (a_ [href_ (url (AuthR LoginR))] "Login"))
    Just {} -> do
      labelledThreads <-
        runDB
          (do threads <-
                E.select
                  (E.from
                     (\thread -> do
                        E.where_
                          (thread E.^. ThreadId `E.notIn`
                           E.subList_select
                             (E.from
                                (\(threadTag, tag) -> do
                                   E.where_
                                     (threadTag E.^. ThreadTagTag E.==. tag E.^.
                                      TagId)
                                   pure (threadTag E.^. ThreadTagThread))))
                        E.orderBy [E.desc (thread E.^. ThreadUpdated)]
                        E.limit 30
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
                             guard
                               (threadTagThread threadTag == entityKey thread)
                             pure label)
                          threadsLabels))
                   threads))
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

listThreads :: (Route App -> Text) -> [(Entity Thread, [Entity Tag])] -> Html ()
listThreads url labelledThreads = do
  p_ [class_ "main-actions"]
    (do a_ [href_ (url InboxR)] "Inbox"
        a_ [href_ (url AllR)] "All")
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
               (toHtml (threadSubject thread))))
    labelledThreads

--------------------------------------------------------------------------------
-- Thread page

getThreadR :: ThreadId -> Handler LucidHtml
getThreadR threadId = do
  (mthread, messages, plainParts, labels) <-
    runDB
      (do labels <-
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
          mthread <- get threadId
          messages <-
            selectList [MessageThread ==. threadId] [Asc MessageReceived]
          plainParts0 <-
            fmap
              (map entityVal)
              (selectList [PlainTextPartMessage <-. map entityKey messages] [])
          plainParts <-
            if null plainParts0
              then fmap
                     (map (toPlainTextPart . entityVal))
                     (selectList [HtmlPartMessage <-. map entityKey messages] [])
              else pure plainParts0
          pure (mthread, messages, plainParts, labels))
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
        (\url ->
           let displayMessage (Entity messageId message) =
                 div_
                   [class_ "message"]
                   (do div_
                         [class_ "message-header"]
                         (do timestamp_ (messageReceived message)
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
                                               (map toHtml ls))))
                                    (groupBy
                                       (on (==) (T.isPrefixOf ">"))
                                       (T.lines
                                          (plainTextPartContent plainTextPart)))))
                            myParts))
                 where
                   myParts =
                     sortBy
                       (comparing plainTextPartOrdering)
                       (filter
                          ((== messageId) . plainTextPartMessage)
                          plainParts)
            in doctypehtml_
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
                              p_ [class_ "thread-actions"]
                                (do a_ [href_ (url InboxR)] "Back to Inbox"
                                    if elem Inbox (map (tagLabel . entityVal . snd) labels)
                                       then a_
                                              [ href_
                                                  (url (RemoveLabelR threadId Inbox))
                                              ]
                                              "Archive"
                                       else a_
                                              [ href_
                                                  (url (ApplyLabelR threadId Inbox))
                                              ]
                                              "Move to Inbox"
                                    a_
                                      [ href_
                                          (url (ApplyLabelR threadId Unread))
                                      ]
                                      "Mark unread")
                              void
                                (displayForest
                                   displayMessage
                                   (fmap
                                      (fmap (\(n, k, _) -> Entity k n))
                                      forest))))))

displayForest :: (a -> Html ()) -> Forest a -> Html ()
displayForest render xs = mapM_ (displayTree render) xs

displayTree :: (a -> Html ()) -> Tree a -> Html ()
displayTree render (Node parent children) = do
  render parent
  div_ [class_ "message-children"] (displayForest render children)

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
                               NodeContent t -> Just t
                               _ -> Nothing)
                            (listify
                               (const True)
                               (parseLT
                                  (LT.fromStrict (htmlPartContent htmlPart))))))))))
    }
  where
    stripBlankLines (x:y:xs) =
      if T.null x && T.null y
        then stripBlankLines (y : xs)
        else x : stripBlankLines (y : xs)
    stripBlankLines x = x

--------------------------------------------------------------------------------
-- Apply label

getApplyLabelR :: ThreadId -> Label -> Handler ()
getApplyLabelR tid label = do
  runDB (labelThread label tid)
  case label of
    Unread -> redirect InboxR
    _ -> redirect (ThreadR tid)

getRemoveLabelR :: ThreadId -> Label -> Handler ()
getRemoveLabelR tid label = do
  runDB (unlabelThread label tid)
  case label of
    Inbox -> redirect InboxR
    _ -> redirect (ThreadR tid)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  ((port, connstr, root, connections), ()) <-
    simpleOptions
      "0.0.0"
      "duta-web"
      "Webmail"
      ((,,,) <$>
       option
         auto
         (metavar "PORT" <> help "Port to listen on" <> short 'p' <> long "port") <*>
       strOption
         (metavar "CONNSTR" <> help "PostgreSQL connection string" <>
          long "connstr") <*>
       strOption
         (metavar "ROOT" <>
          help "App root e.g. https://foo.com (no trailing slash)" <>
          long "approot") <*>
       (option
          auto
          (metavar "COUNT" <> help "Max database connections" <>
           long "max-db-connections" <>
           value 1)))
      empty
  manager <-
    SiteManager <$> fmap T.pack (getEnv "USER") <*> fmap T.pack (getEnv "PASS")
  runStdoutLoggingT
    (withPostgresqlPool
       connstr
       connections
       (\pool -> do
          withResource
            pool
            (runReaderT (runMigration Duta.Types.Model.migrateAll))
          liftIO (warp port (App pool root manager embeddedStatic))))

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
