{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database model.

module Duta.Types.Model where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import Duta.Types.Label
import Duta.Types.Order

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Tag
    label Label
    title Text
    created UTCTime default=CURRENT_TIMESTAMP
    deriving Show
    deriving Eq

  ThreadTag
    thread ThreadId
    tag TagId
    applied UTCTime default=CURRENT_TIMESTAMP
    deriving Show
    deriving Eq

  Thread
    subject Text
    created UTCTime
    updated UTCTime
    messages Int
    deriving Show
    deriving Eq

  Message
    received UTCTime
    thread ThreadId
    parent MessageId Maybe
    identifier Text Maybe
    heloDomain Text
    mailFrom Text
    rcptTo Text
    fromHeader Text Maybe
    toHeader Text Maybe
    subject Text
    ip Text
    deriving Show
    deriving Eq

  MultiPart
    ordering Order
    message MessageId
    parent MultiPartId Maybe
    contentType Text

  PlainTextPart
    ordering Order
    message MessageId
    parent MultiPartId Maybe
    content Text
    deriving Show
    deriving Eq

  HtmlPart
    ordering Order
    message MessageId
    parent MultiPartId Maybe
    content Text
    deriving Show
    deriving Eq

  BinaryPart
    ordering Order
    message MessageId
    parent MultiPartId Maybe
    contentType Text
    name Text Maybe
    content ByteString
    attachment Bool default=TRUE
    deriving Show
    deriving Eq

  OriginalMessage
    message MessageId
    contents ByteString
    deriving Show
    deriving Eq
 |]

data Attachment = Attachment
  { attachmentBinaryPartId :: BinaryPartId
  , attachmentMessageId :: MessageId
  , attachmentName :: Maybe Text
  , attachmentContentType :: Text
  }
