module Main where

import           Control.Monad.Logger
import qualified Duta.SMTP.Receiver as SMTPReceiver
import           System.Environment

main :: IO ()
main = do
  x:y:_ <- getArgs
  runStdoutLoggingT (SMTPReceiver.start y (read x))
