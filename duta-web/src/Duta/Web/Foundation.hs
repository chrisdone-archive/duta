{-# OPTIONS -fno-warn-orphans #-}
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

module Duta.Web.Foundation where

import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Database.Persist.Postgresql as Persistent
import           Development
import           Duta.Types.Label
import           Duta.Types.Model
import           Duta.Web.Types
import           Yesod hiding (toHtml, Html)
import           Yesod.Auth
import           Yesod.Auth.Hardcoded
import           Yesod.Auth.Message (AuthMessage(InvalidLogin))
import           Yesod.EmbeddedStatic

mkYesodData "App" [parseRoutes|
  /static StaticR EmbeddedStatic appStatic
  / InboxR GET
  /all AllR GET
  /deleted DeletedR GET
  /spam SpamR GET
  /auth AuthR Auth getAuth
  /thread/#ThreadId ThreadR GET
  /apply-label/#ThreadId/#Label ApplyLabelR GET
  /remove-label/#ThreadId/#Label RemoveLabelR GET
  /original/#MessageId OriginalR GET
|]

mkEmbeddedStatic development "embeddedStatic" [embedDir "static"]

instance Yesod App where
  addStaticContent = embedStaticContent appStatic StaticR Right
  approot = ApprootMaster appRoot
  maximumContentLength _ _ = Just (1024 * 20)
  authRoute _ = Just $ AuthR LoginR
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized _ _ = do
    authorizedViaHeaders <- authFromHeaders
    case authorizedViaHeaders of
      Just result -> pure result
      Nothing -> do
        mu <- maybeAuthId
        return (maybe AuthenticationRequired (const Authorized) mu)

authFromHeaders :: Handler (Maybe AuthResult)
authFromHeaders = do
  u0 <- lookupHeader "user"
  p0 <- lookupHeader "pass"
  case (,) <$> u0 <*> p0 of
    Just (u, p) -> do
      siteManager <- fmap appSiteManager getYesod
      pure
        (if SiteManager (T.decodeUtf8 u) (T.decodeUtf8 p) == siteManager
           then Just Authorized
           else Just (Unauthorized "Bad user/pass combo."))
    Nothing -> pure Nothing

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
