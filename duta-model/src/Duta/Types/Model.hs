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
import Database.Persist.TH
import Duta.Types.EmailAddress

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Message
    received UTCTime
    from Text
    to Text
    subject Text
    deriving Show
    deriving Eq

  PlainTextPart
    message MessageId
    content Text
    deriving Show
    deriving Eq

  HtmlPart
    message MessageId
    content Text
    deriving Show
    deriving Eq

  BinaryPart
    message MessageId
    contentType Text
    content Text
    deriving Show
    deriving Eq

  OriginalMessage
    message MessageId
    contents ByteString
    deriving Show
    deriving Eq
 |]
