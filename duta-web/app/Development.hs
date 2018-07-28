{-# LANGUAGE CPP #-}

-- | Dev mode settings.

module Development where

-- | Dev flag.
development :: Bool
#ifdef DEVELOPMENT
development = True
#else
development = False
#endif
