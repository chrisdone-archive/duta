-- |

module Duta.Model where

import           Duta.Types.MIME
import qualified Text.Parsec.Rfc2822 as Rfc2822

insertModelMessage :: Monad m => Rfc2822.GenericMessage BodyTree -> m ()
insertModelMessage gm = pure ()
