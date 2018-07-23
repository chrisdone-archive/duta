{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Some support for MIME.

module Duta.MIME where

import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Search as Search
import           Data.Maybe
import           Data.Monoid
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

-- | Given a content type and a body, produce a 'BodyTree'.
parseMessageBodyTree :: Rfc2822.GenericMessage ByteString -> Rfc2822.GenericMessage BodyTree
parseMessageBodyTree msg0@(Rfc2822.Message fields0 _) =
  case extractMultipartBoundary fields0 of
    Nothing -> Rfc2822.Message fields0 (BodyPart (optionalOnly msg0))
    Just {} -> Rfc2822.Message fields0 (go (optionalOnly msg0))
  where
    go msg@(Rfc2822.Message fields content) =
      case extractMultipartBoundary fields of
        Nothing -> BodyPart msg
        Just boundary ->
          BodyMultipart
            (map
               (fmap go . Parsec.parse Rfc2822.message "message part")
               (mapMaybe
                  (\s -> S8.stripPrefix "\r\n" s)
                  (Search.split ("--" <> boundary) content)))
    optionalOnly (Rfc2822.Message fs m) =
      Rfc2822.Message
        (filter
           (\case
              Rfc2822.OptionalField {} -> True
              _ -> False)
           fs)
        m

-- | Extract a multipart boundary from a content type, if it's indeed multi-part.
-- Example: @" multipart/alternative; boundary=\"000000000000e1518b0570a23972\""@
extractMultipartBoundary :: [Rfc2822.Field] -> Maybe ByteString
extractMultipartBoundary fields = do
  s <-
    listToMaybe
      (mapMaybe
         (\case
            Rfc2822.OptionalField "Content-Type" bs -> Just (S8.pack bs)
            _ -> Nothing)
         fields)
  either (const Nothing) pure (Atto8.parseOnly p s)
  where
    p = do
      _ <- Atto8.skipSpace
      _ <- Atto8.string "multipart/"
      _ <- Atto8.takeWhile (\c -> c /= ' ' && c /= ';')
      _ <- Atto8.skipSpace
      _ <- Atto8.char ';'
      _ <- Atto8.skipSpace
      _ <- Atto8.string "boundary=\""
      string <- Atto8.takeWhile (/= '"')
      _ <- Atto8.char '"'
      pure string
