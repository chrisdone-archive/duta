{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString (ByteString)
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Duta
import qualified Network.HaskellNet.SMTP as HaskellNet
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "Integration"
    (do it "Run server" (withServer (pure ()))
        it
          "Run server"
          (withServer
             (HaskellNet.doSMTPPort
                "127.0.0.1"
                (fromIntegral testPort)
                (HaskellNet.sendPlainTextMail
                   "chris@chrisdone.com"
                   "sender@server.com"
                   "subject"
                   "Hello! This is the mail body!")))
        it
          "Regression test against gmail"
          (withServer
             (do xs <-
                   C.runConduit $
                   CL.sourceList gmailInput .| Duta.interaction "" C.yield .|
                   CL.consume
                 shouldBe
                   xs
                   [ Duta.ServiceReady ""
                   , Duta.Okay " OK"
                   , Duta.Okay " OK"
                   , Duta.Okay " OK"
                   , Duta.StartMailInput
                   , Duta.Okay " OK"
                   , Duta.Closing
                   ])))

withServer :: IO a -> IO a
withServer m =
  withAsync
    (Duta.start " wibble" testPort)
    (const (threadDelay (1000 * 100) >> m))

testPort :: Int
testPort = 5870

gmailInput :: [ByteString]
gmailInput =
  [ "EHLO mail-qk0-f170.google.com\r\n"
  , "MAIL FROM:<chrisdone@gmail.com>\r\n"
  , "RCPT TO:<wibble@chrisdone.com>\r\n"
  , "DATA\r\n"
  , "Received: by mail-qk0-f170.google.com with SMTP id b66-v6so11206427qkj.1\r\n        for <wibble@chrisdone.com>; Tue, 10 Jul 2018 03:02:22 -0700 (PDT)\r\nDKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=gmail.com; s=20161025;\r\n        h=mime-version:references:in-reply-to:reply-to:from:date:message-id\r\n         :subject:to;\r\n        bh=vuaco1M4EZ4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=;\r\n        b=NL2PC7xTlI2ampgdRB9B6WzMj/bP+mOvlw/Rtd2+27EMSg5eRdJbs7LjSz8GJV22pl\r\n         hF9C8DLTKRo9BrrE6qs27oPMCG0+/eXwOgBQsw/TR2yjvDT2oBnBRkfVjqakVBhmg2GT\r\n         Ro+iGjDx8vC1136fI/7A3iXNJnjLHAIYcoMfHEljL7s4hqX8jHzQeWG6+W9jLLH08DFS\r\n         IutcEUAnM1DKi8gPP69Qm6i8mEKfHwb8tals8RRthMUhu1w1Hey3djEB5SWpOhU+01BT\r\n         fcGxYD10K5ED+T3FfX6CPC+4PMt/7va2ZD8XEfD2Hiogn1unuhjz++jArvq6jWxIxZmB\r\n         eQrg==\r\nX-Google-DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=1e100.net; s=20161025;\r\n        h=x-gm-message-state:mime-version:references:in-reply-to:reply-to\r\n         :from:date:message-id:subject:to;\r\n        bh=vuaco1M4EZ4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=;\r\n        b=soHCjg9EPG07DTHqu/JJyV4+HDqPVNFuLfE56qtggay+5RheihifZ26n/B6CRUSV0x\r\n         5C9IxFH1FSaPjkEugcW3wfE39zZyUqro+Ozj1IiB0Za0Srw55Iu5YaYyeEBkSY+VVpdY\r\n         NlkNg0/m+gfGo7B05f6CjzqbPQ5iwmOx5kAwS4jKARef0K6Dv2HbeRHYqVCNt66sPGED\r\n         zE6TFPxpbdkrDdSdytTevnu00e8IrqvC9rEft6r/nSrP2i0NpdUEuDa+LCFBWfGWwsuV\r\n         hrVAtkCTfPOGgJpFCZ+b8uWWqovhnkfWrx0TE0uksW64YqdJXIH9VXGhnWRletjnz6+C\r\n         AAyg==\r\nX-Gm-Message-State: APt69E11MY8bREVIjYEX2S7HZECtZlfMQ8zqcH1I2F0mDl6fWrrR3grG\r\n\tyhZSXOzxXzHWHKhJEGAtSJyYMDBP063Jz9UTjhaiNQ==\r\nX-Google-Smtp-Source: AAOMgpeSPyQt6gLoYRDkyjOpo2rN1aBvzv1SdKcmo5DCACWDqPLCUXyocBsaxfYTTsKYRkJp2O9D67xiJMxr5asnlF0=\r\nX-Received: by 2002:a37:1028:: with SMTP id a40-v6mr21175009qkh.257.1531216941632;\r\n Tue, 10 Jul 2018 03:02:21 -0700 (PDT)\r\nMIME-Version: 1.0\r\nReferences: <CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubWTWSg@mail.gmail.com>\r\nIn-Reply-To: <CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubWTWSg@mail.gmail.com>\r\nReply-To: chrisdone@googlemail.com\r\nFrom: Christopher Done <chrisdone@gmail.com>\r\nDate: Tue, 10 Jul 2018 11:02:10 +0100\r\nMessage-ID: <CAAJHNPCnR2LVyN+Ns5TauNTC9Gb1hVnUHGD8+fKstAqm_5yvQA@mail.gmail.com>\r\nSubject: Re: wibbling\r\nTo: wibble@chrisdone.com\r\nContent-Type: multipart/alternative; boundary=\"000000000000e1518b0570a23972\"\r\n\r\n--000000000000e1518b0570a23972\r\nContent-Type: text/plain; charset=\"UTF-8\"\r\n\r\nwe continue!\r\n\r\nOn Tue, 10 Jul 2018 at 10:36, Christopher Done <chrisdone@gmail.com> wrote:\r\n\r\n> hey wibble\r\n>\r\n\r\n--000000000000e1518b0570a23972\r\nContent-Type: text/html; charset=\"UTF-8\"\r\nContent-Transfer-Encoding: quoted-printable\r\n\r\n<div dir=3D\"ltr\">we continue!</div><br><div class=3D\"gmail_quote\"><div dir=\r\n=3D\"ltr\">On Tue, 10 Jul 2018 at 10:36, Christopher Done &lt;<a href=3D\"mail=\r\nto:chrisdone@gmail.com\">chrisdone@gmail.com</a>&gt; wrote:<br></div><blockq=\r\nuote class=3D\"gmail_quote\" style=3D\"margin:0 0 0 .8ex;border-left:1px #ccc =\r\nsolid;padding-left:1ex\"><div dir=3D\"ltr\">hey wibble</div>\r\n</blockquote></div>\r\n\r\n--000000000000e1518b0570a23972--\r\n.\r\n"
  , "QUIT\r\n"
  ]
