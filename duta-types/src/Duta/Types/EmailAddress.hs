{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Wrapper around the already existing EmailAddress type.

module Duta.Types.EmailAddress
  ( EmailAddress
  , emailAddressLocal
  , emailAddressDomain
  ) where

import qualified Data.Attoparsec.ByteString as Atto
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.Persist.Sql
import           Text.Email.Parser

emailAddressLocal :: EmailAddress -> ByteString
emailAddressLocal = localPart

emailAddressDomain :: EmailAddress -> ByteString
emailAddressDomain = domainPart

instance PersistField EmailAddress where
  toPersistValue = PersistText . T.decodeUtf8 . toByteString
  fromPersistValue =
    \case
      PersistText e ->
        first
          (<> (": value was: " <> T.pack (show e)))
          (first T.pack (Atto.parseOnly addrSpec (T.encodeUtf8 e)))
      _ -> Left "Need PersistText field from DB."

instance PersistFieldSql EmailAddress where
  sqlType _ = SqlString
