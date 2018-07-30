-- |

module Duta.Web.Types where

import Data.Pool
import Data.Text (Text)
import Database.Persist.Postgresql as Persistent
import Yesod.EmbeddedStatic

data SiteManager = SiteManager
  { manUserName :: Text
  , manPassWord :: Text
  } deriving (Show, Eq)

data App = App
  { appPool :: Pool SqlBackend
  , appRoot :: Text
  , appSiteManager :: SiteManager
  , appStatic :: EmbeddedStatic
  }
