{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database model.

module Duta.Types.Model where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Time
import           Database.Persist.TH
import           Duta.Types.EmailAddress

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Message
    received UTCTime
    authored UTCTime
    from EmailAddress
    to EmailAddress
    subject Text

  PlainTextPart
    message MessageId
    content Text

  HtmlPart
    message MessageId
    content ByteString

  BinaryPart
    message MessageId
    contentType Text
    transferEncoding Text

  OriginalMessage
    message MessageId
    contents ByteString
 |]
