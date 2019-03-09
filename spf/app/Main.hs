{-# LANGUAGE OverloadedStrings #-}
import qualified Spf
main :: IO ()
main = do
  serv <- Spf.newSpfServer Spf.SPF_DNS_CACHE Spf.ModerateLevel
  result <-
    Spf.makeSpfRequest
      serv
      $  Spf.SpfRequest "142.11.238.122" "hwc-hwp-4465340" "rbenson825@chrisdone.com"

  print result

-- Real examples
--
-- chrisdone@fpcomplete.com                     | 209.85.166.179 | mail-it1-f179.google.com

-- duta=> select mail_from, ip, helo_domain from message where mail_from like '%bazon%' order by received desc limit 3;
--     mail_from    |       ip       | helo_domain
-- -----------------+----------------+--------------
--  mihai@bazon.net | 144.76.241.201 | somemore.net
