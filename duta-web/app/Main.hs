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
import           Data.Pool
import           Data.Text (Text)
import           Database.Persist.Postgresql
import qualified Duta.Types.Model
import           Lucid
import           Options.Applicative.Simple
import           Yesod
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
  / HomeR GET
  /auth AuthR Auth getAuth
|]

instance Yesod App where
  approot = ApprootMaster appRoot
  maximumContentLength _ _ = Just (1024 * 20)
  authRoute _ = Just $ AuthR LoginR
  isAuthorized HomeR _ = do
    mu <- maybeAuthId
    return (maybe AuthenticationRequired (const Authorized) mu)
  isAuthorized _ _ = pure Authorized

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB act = do
    App pool _ <- getYesod
    runSqlPool act pool

instance YesodAuth App where
  type AuthId App = Text
  loginDest _ = HomeR
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
-- Raw content

getHomeR :: Handler LucidHtml
getHomeR = do
  mu <- maybeAuthId
  lucid
    (\url -> do
       case mu of
         Nothing -> p_ (a_ [href_ (url (AuthR LoginR))] "Login")
         Just {} -> p_ (a_ [href_ (url (AuthR LogoutR))] "Logout")
       p_ (Lucid.toHtml (show mu)))

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
