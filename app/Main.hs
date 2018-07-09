module Main where

import qualified Duta
import           System.Environment

main :: IO ()
main = do x:y:_ <- getArgs
          Duta.start y (read x)
