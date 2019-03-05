-- |

module Duta.SMTP.RFC2047 where

import qualified Data.Attoparsec.ByteString as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Network.Email.Header.Parser as HeaderParser

decodeRFC2047 :: ByteString -> Text
decodeRFC2047 i =
  either
    (const (either (const (T.pack (S8.unpack i))) id (T.decodeUtf8' i)))
    LT.toStrict
    (Atto.parseOnly HeaderParser.unstructured i)
