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
import           Data.Graph
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Database.Persist.Postgresql
import           Development
import           Duta.Types.Model
import           Lucid
import           Options.Applicative.Simple
import           System.Environment
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
  /auth AuthR Auth getAuth
  /thread/#ThreadId ThreadR GET
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
      threads <-
        runDB (selectList [ThreadArchived ==. False] [Desc ThreadUpdated])
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
                          mapM_
                            (\(Entity threadId thread) -> do
                               div_
                                 [class_ "thread-preview"]
                                 (do timestamp_ (threadUpdated thread)
                                     span_
                                       [class_ "messages-count"]
                                       (do "("
                                           toHtml (show (threadMessages thread))
                                           ")")
                                     a_
                                       [ class_ "thread-preview-link"
                                       , href_ (url (ThreadR threadId))
                                       ]
                                       (toHtml (threadSubject thread))))
                            threads))))

--------------------------------------------------------------------------------
-- Thread page

getThreadR :: ThreadId -> Handler LucidHtml
getThreadR threadId = do
  (mthread, messages, plainParts) <-
    runDB
      (do mthread <- get threadId
          messages <-
            selectList [MessageThread ==. threadId] [Asc MessageReceived]
          plainParts <-
            fmap
              (map entityVal)
              (selectList [PlainTextPartMessage <-. map entityKey messages] [])
          pure (mthread, messages, plainParts))
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
