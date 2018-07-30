{-# LANGUAGE OverloadedStrings #-}

-- |

module Duta.Types.Label where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist.Sql
import           Yesod.Core.Dispatch

instance PersistFieldSql Label where
  sqlType _ = SqlString

instance PersistField Label where
  toPersistValue = toPersistValue . labelSlug
  fromPersistValue (PersistText t) = parseSlug t
  fromPersistValue _ = Left "Label slug should be text."

-- | A thread label.
data Label
  = Inbox
  | Sent
  | Unread
  | Deleted
  | Muted
  | Spam
  | Other Text
  deriving (Show, Eq)

instance PathPiece Label where
  fromPathPiece = either (const Nothing) Just . parseSlug
  toPathPiece = labelSlug

instance Read Label where
  readsPrec _ = \s -> [(l, "") | Right l <- [parseSlug (T.pack s)]]

parseSlug :: Text -> Either Text Label
parseSlug t =
  case t of
    "inbox" -> pure Inbox
    "sent" -> pure Sent
    "unread" -> pure Unread
    "deleted" -> pure Deleted
    "muted" -> pure Muted
    _
      | T.all ((||) <$> isDigit <*> isLower) t -> pure (Other t)
    _ -> Left "Invalid slug for label. Should be lowercase alphanumeric."

labelSlug :: Label -> Text
labelSlug l =
  case l of
    Inbox -> "inbox"
    Unread -> "unread"
    Sent -> "sent"
    Muted -> "muted"
    Deleted -> "deleted"
    Spam -> "spam"
    Other t -> t

labelTitle :: Label -> Text
labelTitle l =
  case l of
    Inbox -> "Inbox"
    Unread -> "Unread"
    Sent -> "Sent"
    Muted -> "Muted"
    Deleted -> "Deleted"
    Spam -> "spam"
    Other t -> t
