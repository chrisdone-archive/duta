{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module Duta.Types.Order where

import Database.Persist.Class
import Database.Persist.Sql


-- | Ordering of posts.
newtype Order = Order Int
  deriving (Ord, Show, Eq, PersistField, PersistFieldSql)

incOrder :: Order -> Order
incOrder (Order n) = Order (n + 1)
