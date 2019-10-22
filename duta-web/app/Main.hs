{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Char
import Data.List
import Data.Pool
import Database.Persist.Postgresql as Persistent
import Duta.Types.Model
import Duta.Web ()
import Duta.Web.Foundation
import Duta.Web.Types
import Network.Wai.Handler.Warp (defaultSettings, run)
import Network.Wai.Handler.Warp.Internal
import Network.Wai.Handler.WarpTLS (tlsSettingsChain, runTLS)
import Options.Applicative.Simple
import System.Directory
import System.EnvArgs
import System.Environment
import Yesod hiding (toHtml, Html)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  ((port, connstr, root, connections, user, pass, tls), ()) <-
    withEnvArgs
      "DUTA_WEB_"
      (simpleOptions
         "0.0.0"
         "duta-web"
         "Webmail"
         ((,,,,,,) <$>
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
            (metavar "PASS" <> help "Password for web server" <> long "password") <*>
          (let debug =
                 flag'
                   (Left True)
                   (long "insecure-http" <> help "Insecure HTTP mode")
               tlsConfig =
                 (,,) <$>
                 strOption
                   (metavar "CERT_FILE" <> help "Certificate file" <>
                    long "cert-file") <*>
                 some
                   (strOption
                      (metavar "CHAIN_FILE" <> help "Chain file" <>
                       long "chain-file")) <*>
                 strOption
                   (metavar "KEY_FILE" <> help "Key file" <> long "key-file")
            in debug <|> fmap Right tlsConfig))
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
          liftIO
            (do application <- toWaiApp (App pool root manager embeddedStatic)
                let settings = defaultSettings {settingsPort = port}
                case tls of
                  Left {} -> run port application
                  Right (cert, chain, key) ->
                    forever
                      (do exists <- doesFileExist cert
                          if exists
                            then runTLS
                                   (tlsSettingsChain cert chain key)
                                   settings
                                   application
                            else threadDelay (1000 * 1000 * 1)))))
