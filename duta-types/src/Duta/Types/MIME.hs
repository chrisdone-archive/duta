{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |

module Duta.Types.MIME where

import           Data.ByteString (ByteString)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Rfc2822 as Rfc2822

-- | A tree representing the body of a message.
data BodyTree
  = BodyMultipart ![Either Parsec.ParseError BodyTree]
  | BodyPart !(Rfc2822.GenericMessage ByteString)
  | Nul
  deriving (Eq, Show)

deriving instance Eq a => Eq (Rfc2822.GenericMessage a)
deriving instance Eq Rfc2822.Field
