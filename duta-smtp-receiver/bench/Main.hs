{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

-- |

module Main where

import           Codec.MIME.Parse
import           Codec.MIME.Type
import           Control.DeepSeq
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import           GHC.Generics

main :: IO ()
main = do
  bytes <- S.readFile "558.txt"
  let !_ = force (parseMIMEMessage (T.pack (S8.unpack bytes)))
  putStrLn "OK!"

instance NFData MIMEValue
deriving instance Generic MIMEValue
instance NFData MIMEContent
deriving instance Generic MIMEContent
instance NFData MIMEParam
deriving instance Generic MIMEParam
instance NFData MIMEType
deriving instance Generic MIMEType
instance NFData Type
deriving instance Generic Type
instance NFData DispType
deriving instance Generic DispType
instance NFData DispParam
deriving instance Generic DispParam
instance NFData Multipart
deriving instance Generic Multipart
instance NFData Disposition
deriving instance Generic Disposition
