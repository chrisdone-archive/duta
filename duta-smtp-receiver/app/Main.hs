{-# LANGUAGE OverloadedStrings #-}

-- | Main SMTP receiver program.

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.Monoid
import           Data.Pool
import           Data.Time
import           Database.Persist.Postgresql
import           Duta.Model
import           Duta.SMTP.Receiver
import qualified Duta.Types.Model
import           Options.Applicative.Simple

main :: IO ()
main = do
  ((host, port, connstr, connections), ()) <-
    simpleOptions
      "0.0.0"
      "duta-smtp-receiver"
      "SMTP receiver"
      ((,,,) <$>
       strOption
         (metavar "HOSTNAME" <> help "Hostname to listen on" <> long "hostname") <*>
       option
         auto
         (metavar "PORT" <> help "Port to listen on" <> short 'p' <> long "port") <*>
       strOption
         (metavar "CONNSTR" <> help "PostgreSQL connection string" <>
          long "connstr") <*>
       (option
          auto
          (metavar "COUNT" <> help "Max database connections" <>
           long "max-db-connections" <>
           value 1)))
      empty
  runNoLoggingT
    (withPostgresqlPool
       connstr
       connections
       (\pool ->
          liftIO
            (do withResource
                  pool
                  (runReaderT (runMigration Duta.Types.Model.migrateAll))
                runStdoutLoggingT
                  (start
                     Start
                       { startHostname = host
                       , startPort = port
                       , startOnMessage =
                           \(From from to) bs msg -> do
                             now <- liftIO getCurrentTime
                             logDebug "Doing database insert."
                             filterLogger
                               (\_src _level -> False)
                               (withResource
                                  pool
                                  (runReaderT (insertModelMessage now msg bs)))
                             logDebug "Done database insert."
                       , startPool = pool
                       }))))
