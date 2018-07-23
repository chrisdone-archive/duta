module Main where

import           Control.Monad.Logger.CallStack
import qualified Duta.SMTP.Receiver
import           System.Environment

main :: IO ()
main = do
  port:hostname:_ <- getArgs
  runStdoutLoggingT (Duta.SMTP.Receiver.startWithDB (read port) hostname)
