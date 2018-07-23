-- |  A mailbox receives mail.  It is a conceptual entity which does not
-- necessarily pertain to file storage.  For example, some sites may
-- choose to print mail on a printer and deliver the output to the
-- addressee's desk.  Normally, a mailbox is comprised of two parts: (1)
-- an optional display name that indicates the name of the recipient
-- (which could be a person or a system) that could be displayed to the
-- user of a mail application, and (2) an addr-spec address enclosed in
-- angle brackets ("<" and ">").  There is also an alternate simple form
-- of a mailbox where the addr-spec address appears alone, without the
-- recipient's name or the angle brackets.

module Duta.Types.Mailbox where

import Data.Text (Text)
import Duta.Types.EmailAddress

data Mailbox = Mailbox
  { mailboxEmail :: !EmailAddress
  , mailboxTitle :: !Text
  } deriving (Show, Eq, Ord)
