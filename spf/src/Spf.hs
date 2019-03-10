{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | SPF binding.

module Spf
  ( newServer
  , DnsType(..)
  , DebugLevel(..)
  , Server
  , makeRequest
  , Request(..)
  , SpfException(..)
  , RequestResult(..)
  ) where

import           Control.Exception
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Coerce
import           Data.Typeable
import           Foreign.C
import           Foreign.ForeignPtr
import           Foreign.Ptr

-- | An abnormal situation.
data SpfException =
  AllocationReturnedNull !String
  deriving (Typeable, Show, Eq)
instance Exception SpfException

--------------------------------------------------------------------------------
-- Spf requests

data RequestResult
  = SPF_REQUEST_ALLOC_FAIL
  | SPF_REQUEST_INVALID_IP
  | SPF_REQUEST_INVALID_HELO_DOMAIN
  | SPF_REQUEST_INVALID_ENVELOPE_FROM
  | SPF_REQUEST_RESULT_INVALID
  | SPF_REQUEST_RESULT_NEUTRAL
  | SPF_REQUEST_RESULT_PASS
  | SPF_REQUEST_RESULT_FAIL
  | SPF_REQUEST_RESULT_SOFTFAIL
  | SPF_REQUEST_RESULT_NONE
  | SPF_REQUEST_RESULT_TEMPERROR
  | SPF_REQUEST_RESULT_PERMERROR
  deriving (Enum, Eq, Ord, Show)

data Request =
  Request
    { requestIpV4 :: !ByteString
    , requestHeloDomain :: !ByteString
    , requestFromAddress :: !ByteString
    }

-- | Make an SPF request and return a simple enum type.
makeRequest :: Server -> Request -> IO RequestResult
makeRequest (Server fptr) req =
  fmap
    toEnum
    (withForeignPtr
       fptr
       (\ptr ->
          S.useAsCString
            (requestIpV4 req)
            (\ipv4 ->
               S.useAsCString
                 (requestFromAddress req)
                 (\sender ->
                    S.useAsCString
                      (requestHeloDomain req)
                      (\helo -> spf_make_request ptr ipv4 helo sender)))))

foreign import ccall "spf2 spf_make_request" spf_make_request
  :: Ptr SPF_server_t
  -> CString
  -> CString
  -> CString
  -> IO Int

--------------------------------------------------------------------------------
-- Server

-- | An SPF server.
newtype Server =
  Server (ForeignPtr SPF_server_t)
  deriving (Eq)

data DebugLevel
  = SilentLevel -- ^ 0: Be completely silent, no debugging information will be generated.
  | ModerateLevel -- ^ 1: Moderate amount of debugging information.
  | DebugLevelNoResult
  -- ^ 2: Include some detailed information about the DNS lookups. Usually this is not needed.
  --
  -- HASKELL AUTHOR NOTE: For some reason, setting DebugLevel
  -- generates a result of SPF_REQUEST_RESULT_NONE. So don't use this
  -- and expect a result. This is why this constructor is suffixed
  -- with "NoResult".
  deriving (Eq, Enum, Bounded, Show, Ord)

-- | Type of DNS to use.
data DnsType
  = SPF_DNS_RESOLV
  | SPF_DNS_CACHE
  | SPF_DNS_ZONE
  deriving (Enum, Eq, Ord, Bounded, Show)

-- | Make a new SPF server.
newServer :: DnsType -> DebugLevel -> IO Server
newServer ty debugLevel =
  fmap
    coerce
    (uninterruptibleMask_
       -- This procedure is safe from async exceptions and safe from
       -- double frees.
       (do ptr <-
             assertNotNull
               "SPF_server_new"
               (spf_SPF_server_new (fromEnum ty) (fromEnum debugLevel))
           newForeignPtr spf_SPF_server_free ptr))

-- | Internal server type.
data SPF_server_t

foreign import ccall "spf2 SPF_server_new" spf_SPF_server_new
  :: Int -> Int -> IO (Ptr SPF_server_t)

foreign import ccall "spf2 &SPF_server_free" spf_SPF_server_free
  :: FunPtr (Ptr SPF_server_t -> IO ())

--------------------------------------------------------------------------------
-- FFI helpers

-- | Check that the RETCODE is successful.
assertNotNull :: (Coercible a (Ptr ())) => String -> IO a -> IO a
assertNotNull label m = do
  val <- m
  if coerce val == nullPtr
    then throwIO (AllocationReturnedNull label)
    else pure val
