module Main where

import qualified Duta
import           System.Environment

main :: IO ()
main = do x:_ <- getArgs
          Duta.start (read x)
