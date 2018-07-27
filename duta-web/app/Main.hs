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
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist.Postgresql
import           Duta.Types.Model
import           Lucid
import           Options.Applicative.Simple
import           Yesod hiding (toHtml)
import           Yesod.Auth
import           Yesod.Auth.Hardcoded
import           Yesod.Auth.Message (AuthMessage(InvalidLogin))
import           Yesod.Lucid

--------------------------------------------------------------------------------
-- App

data App = App
  { appPool :: Pool SqlBackend
  , appRoot :: Text
  }

mkYesod "App" [parseRoutes|
  / InboxR GET
  /auth AuthR Auth getAuth
  /thread/#ThreadId ThreadR GET
|]

instance Yesod App where
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
    App pool _ <- getYesod
    runSqlPool act pool

instance YesodAuth App where
  type AuthId App = Text
  loginDest _ = InboxR
  logoutDest _ = AuthR LoginR
  authPlugins _ = [authHardcoded]
  authenticate Creds {..} =
    pure
      (case credsPlugin of
         "hardcoded" ->
           case lookupUser credsIdent of
             Nothing -> UserError InvalidLogin
             Just m -> Authenticated (manUserName m)
         _ -> ServerError "Invalid auth plugin.")

instance YesodAuthHardcoded App where
  validatePassword u = return . validPassword
    where validPassword p =
            isJust (find (\m -> manUserName m == u && manPassWord m == p) siteManagers)
  doesUserNameExist  = return . isJust . lookupUser

instance YesodAuthPersist App where
  type AuthEntity App = SiteManager
  getAuthEntity = pure . lookupUser

lookupUser :: Text -> Maybe SiteManager
lookupUser username = find (\m -> manUserName m == username) siteManagers

--------------------------------------------------------------------------------
-- Accounts

data SiteManager = SiteManager
  { manUserName :: Text
  , manPassWord :: Text }
  deriving Show

siteManagers :: [SiteManager]
siteManagers = [SiteManager "chris" "donkey"]

--------------------------------------------------------------------------------
-- Inbox page

getInboxR :: Handler LucidHtml
getInboxR = do
  mu <- maybeAuthId
  case mu of
    Nothing -> lucid (\url -> p_ (a_ [href_ (url (AuthR LoginR))] "Login"))
    Just {} -> do
      threads <-
        runDB (selectList [ThreadArchived ==. False] [Asc ThreadUpdated])
      lucid
        (\url -> do
           p_ (a_ [href_ (url (AuthR LogoutR))] "Logout")
           h1_ "Inbox"
           mapM_
             (\(Entity threadId thread) -> do
                p_
                  (do a_
                        [href_ (url (ThreadR threadId))]
                        (toHtml (threadSubject thread))
                      br_ []
                      em_ (toHtml (show (threadUpdated thread)))))
             threads)

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
  case mthread of
    Nothing -> notFound
    Just thread ->
      lucid
        (\_url ->
           let displayMessage (Entity messageId message) = do
                 p_ (em_ (toHtml (show (messageReceived message))))
                 p_
                   (do strong_ "From: "
                       toHtml (messageFrom message))
                 p_
                   (do strong_ "To: "
                       toHtml (messageTo message))
                 mapM_
                   (\plainTextPart ->
                      mapM_
                        (p_ . toHtml)
                        (T.lines (plainTextPartContent plainTextPart)))
                   myParts
                 where
                   myParts =
                     sortBy
                       (comparing plainTextPartOrdering)
                       (filter
                          ((== messageId) . plainTextPartMessage)
                          plainParts)
            in do h1_ (toHtml (threadSubject thread))
                  mapM_ displayMessage messages)

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
         (metavar "ROOT" <> help "App root e.g. https://foo.com (no trailing slash)" <>
          long "approot") <*>
       (option
          auto
          (metavar "COUNT" <> help "Max database connections" <>
           long "max-db-connections" <>
           value 1)))
      empty
  runStdoutLoggingT
    (withPostgresqlPool
       connstr
       connections
       (\pool -> do
          withResource
            pool
            (runReaderT (runMigration Duta.Types.Model.migrateAll))
          liftIO (warp port (App pool root))))
