module Main where

import           Control.Monad.Logger
import qualified Duta
import           System.Environment

main :: IO ()
main = do
  x:y:_ <- getArgs
  runStdoutLoggingT (Duta.start y (read x))
