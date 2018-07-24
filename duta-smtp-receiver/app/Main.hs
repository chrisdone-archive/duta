-- | Main SMTP receiver program.

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.Monoid
import           Data.Pool
import           Data.Time
import           Database.Persist.Sqlite
import           Duta.Model
import           Duta.SMTP.Receiver
import qualified Duta.Types.Model
import           Options.Applicative.Simple

main :: IO ()
main = do
  ((host, port, dbfile, connections), ()) <-
    simpleOptions
      "0.0.0"
      "sift"
      "Sift through Haskell modules"
      ((,,,) <$>
       strOption
         (metavar "HOSTNAME" <> help "Hostname to listen on" <> long "hostname") <*>
       option
         auto
         (metavar "PORT" <> help "Port to listen on" <> short 'p' <> long "port") <*>
       strOption
         (metavar "FILE" <> help "SQLite database file" <> long "sqlite-file") <*>
       (option
          auto
          (metavar "COUNT" <> help "Max database connections" <>
           long "max-db-connections" <>
           value 1)))
      empty
  runStdoutLoggingT
    (withSqlitePoolInfo
       (mkSqliteConnectionInfo dbfile)
       connections
       (\pool ->
          do withResource pool (runReaderT (runMigration Duta.Types.Model.migrateAll))
             start
               Start
                 { startHostname = host
                 , startPort = port
                 , startOnMessage =
                     \msg -> do
                       now <- liftIO getCurrentTime
                       insertModelMessage now msg
                 , startPool = pool
                 }))
