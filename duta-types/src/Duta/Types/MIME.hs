{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |

module Duta.Types.MIME where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Rfc2822 as Rfc2822

-- | A tree representing the body of a message.
data BodyTree a
  = BodyMultipart ![Either Parsec.ParseError (BodyTree a)]
  | BodyPart !a
  deriving (Eq, Show, Functor, Traversable, Foldable)

deriving instance Eq a => Eq (Rfc2822.GenericMessage a)
deriving instance Eq Rfc2822.Field
