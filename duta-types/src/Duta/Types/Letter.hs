-- |

module Duta.Types.Letter where

import Codec.MIME.Type
import Data.ByteString (ByteString)
import Data.Time

data Letter =
  Letter
    { letterFrom :: ByteString
    , letterTo :: ByteString
    , letterPayload :: ByteString
    , letterMimeValue :: MIMEValue
    , letterReceived :: UTCTime
    }
