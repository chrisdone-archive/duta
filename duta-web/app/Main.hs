{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Char
import           Data.List
import           Data.Pool
import           Database.Persist.Postgresql as Persistent
import           Duta.Types.Model
import           Duta.Web ()
import           Duta.Web.Foundation
import           Duta.Web.Types
import           Options.Applicative.Simple
import           System.Environment
import           Yesod hiding (toHtml, Html)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  ((port, connstr, root, connections, user, pass), ()) <-
    envToArgs
      (simpleOptions
         "0.0.0"
         "duta-web"
         "Webmail"
         ((,,,,,) <$>
          option
            auto
            (metavar "PORT" <> help "Port to listen on" <> short 'p' <>
             long "port") <*>
          strOption
            (metavar "CONNSTR" <> help "PostgreSQL connection string" <>
             long "connstr") <*>
          strOption
            (metavar "ROOT" <>
             help "App root e.g. https://foo.com (no trailing slash)" <>
             long "approot") <*>
          option
            auto
            (metavar "COUNT" <> help "Max database connections" <>
             long "max-db-connections" <>
             value 1) <*>
          strOption (metavar "USER" <> help "Web username" <> long "username") <*>
          strOption
            (metavar "PASS" <> help "Password for web server" <> long "password"))
         empty)
  manager <- SiteManager <$> pure user <*> pure pass
  runStdoutLoggingT
    (withPostgresqlPool
       connstr
       connections
       (\pool -> do
          withResource
            pool
            (runReaderT (runMigration Duta.Types.Model.migrateAll))
          liftIO (warp port (App pool root manager embeddedStatic))))

envToArgs :: IO b -> IO b
envToArgs m = do
  env <- getEnvironment
  args <- getArgs
  withArgs
    (args ++
     concatMap
       (\(k, v) -> maybe [] (\arg -> ["--" ++ map toLower arg, v]) (stripPrefix prefix k))
       env)
    m
  where
    prefix = "DUTA_WEB_"
