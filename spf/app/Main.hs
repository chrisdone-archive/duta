{-# LANGUAGE OverloadedStrings #-}
import qualified Spf
main :: IO ()
main = do
  serv <- Spf.newSpfServer Spf.SPF_DNS_CACHE Spf.ModerateLevel
  result <-
    Spf.makeSpfRequest
      serv
      (Spf.SpfRequest
         { Spf.spfRequestIpV4 = "130.211.0.0"
         , Spf.spfRequestHeloDomain = "mail.gmail.com"
         , Spf.spfRequestFrom = "gmail.com"
         })
  print result
