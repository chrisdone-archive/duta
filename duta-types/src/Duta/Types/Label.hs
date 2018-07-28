{-# LANGUAGE OverloadedStrings #-}

-- |

module Duta.Types.Label where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist.Sql

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
  | Other Text
  deriving (Show, Eq)

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
    Other t -> t

labelTitle :: Label -> Text
labelTitle l =
  case l of
    Inbox -> "Inbox"
    Unread -> "Unread"
    Sent -> "Sent"
    Muted -> "Muted"
    Deleted -> "Deleted"
    Other t -> t
