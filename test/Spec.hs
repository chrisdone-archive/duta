{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.Logger
import           Data.ByteString (ByteString)
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Time
import qualified Duta.Receiver as Duta
import           Network.Mail.Parse
import           Network.Mail.Parse.Types
import           System.Time
import           Test.Hspec
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Rfc2822 as Rfc2822

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "Integration"
    (do it
          "Regression test against GMail"
          (do xs <-
                runNoLoggingT
                  (C.runConduit $
                   CL.sourceList gmailInput .| Duta.interaction "" C.yield .|
                   CL.consume)
              shouldBe
                xs
                [ Duta.ServiceReady ""
                , Duta.Okay " OK"
                , Duta.Okay " OK"
                , Duta.Okay " OK"
                , Duta.StartMailInput
                , Duta.Okay " OK"
                , Duta.Closing
                ])
        it
          "Regression test against Postfix"
          (do xs <-
                runNoLoggingT $
                C.runConduit $
                CL.sourceList postfixInput .| Duta.interaction "" C.yield .|
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
                ]))
  describe
    "RFC 2822 Parser"
    (do it
          "GMail example"
          (shouldBe
             (Parsec.parse Rfc2822.message "" gmailMessage)
             (Right gmailMessageParsed))
        it
          "Postfix example"
          (shouldBe
             (Parsec.parse Rfc2822.message "" postfixMessage)
             (Right postfixMessageParsed))
        it
          "GMail example (emailparse)"
          (shouldBe
             (Network.Mail.Parse.parseMessage gmailMessage)
             (Right gmailMessageParsed2))
        it
          "Postfix example (emailparse)"
          (shouldBe
             (Network.Mail.Parse.parseMessage postfixMessage)
             (Right postfixMessageParsed2)))

deriving instance Eq a => Eq (Rfc2822.GenericMessage a)
deriving instance Eq Rfc2822.Field

gmailInput :: [ByteString]
gmailInput =
  [ "EHLO mail-qk0-f170.google.com\r\n"
  , "MAIL FROM:<chrisdone@gmail.com>\r\n"
  , "RCPT TO:<wibble@chrisdone.com>\r\n"
  , "DATA\r\n"
  , gmailMessage
  , "QUIT\r\n"
  ]

postfixInput :: [ByteString]
postfixInput =
  [ "EHLO somemore.net\r\n"
  , "MAIL FROM:<mihai@bazon.net>\r\n"
  , "RCPT TO:<blah@chrisdone.com>\r\n"
  , "DATA\r\n"
  , postfixMessage
  , "QUIT\r\n"
  ]

gmailMessageParsed :: Rfc2822.GenericMessage ByteString
gmailMessageParsed =
  (Rfc2822.Message
     [ Rfc2822.Received
         ( [ ("by", "mail-qk0-f170.google.com")
           , ("with", "SMTP")
           , ("id", "b66-v6so11206427qkj.1")
           , ("for", "wibble@chrisdone.com")
           ]
         , CalendarTime
             { ctYear = 2018
             , ctMonth = July
             , ctDay = 10
             , ctHour = 3
             , ctMin = 2
             , ctSec = 22
             , ctPicosec = 0
             , ctWDay = Tuesday
             , ctYDay = 0
             , ctTZName = ""
             , ctTZ = -25200
             , ctIsDST = False
             })
     , Rfc2822.OptionalField
         "DKIM-Signature"
         " v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=gmail.com; s=20161025;\r\n        h=mime-version:references:in-reply-to:reply-to:from:date:message-id\r\n         :subject:to;\r\n        bh=vuaco1M4EZ4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=;\r\n        b=NL2PC7xTlI2ampgdRB9B6WzMj/bP+mOvlw/Rtd2+27EMSg5eRdJbs7LjSz8GJV22pl\r\n         hF9C8DLTKRo9BrrE6qs27oPMCG0+/eXwOgBQsw/TR2yjvDT2oBnBRkfVjqakVBhmg2GT\r\n         Ro+iGjDx8vC1136fI/7A3iXNJnjLHAIYcoMfHEljL7s4hqX8jHzQeWG6+W9jLLH08DFS\r\n         IutcEUAnM1DKi8gPP69Qm6i8mEKfHwb8tals8RRthMUhu1w1Hey3djEB5SWpOhU+01BT\r\n         fcGxYD10K5ED+T3FfX6CPC+4PMt/7va2ZD8XEfD2Hiogn1unuhjz++jArvq6jWxIxZmB\r\n         eQrg=="
     , Rfc2822.OptionalField
         "X-Google-DKIM-Signature"
         " v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=1e100.net; s=20161025;\r\n        h=x-gm-message-state:mime-version:references:in-reply-to:reply-to\r\n         :from:date:message-id:subject:to;\r\n        bh=vuaco1M4EZ4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=;\r\n        b=soHCjg9EPG07DTHqu/JJyV4+HDqPVNFuLfE56qtggay+5RheihifZ26n/B6CRUSV0x\r\n         5C9IxFH1FSaPjkEugcW3wfE39zZyUqro+Ozj1IiB0Za0Srw55Iu5YaYyeEBkSY+VVpdY\r\n         NlkNg0/m+gfGo7B05f6CjzqbPQ5iwmOx5kAwS4jKARef0K6Dv2HbeRHYqVCNt66sPGED\r\n         zE6TFPxpbdkrDdSdytTevnu00e8IrqvC9rEft6r/nSrP2i0NpdUEuDa+LCFBWfGWwsuV\r\n         hrVAtkCTfPOGgJpFCZ+b8uWWqovhnkfWrx0TE0uksW64YqdJXIH9VXGhnWRletjnz6+C\r\n         AAyg=="
     , Rfc2822.OptionalField
         "X-Gm-Message-State"
         " APt69E11MY8bREVIjYEX2S7HZECtZlfMQ8zqcH1I2F0mDl6fWrrR3grG\r\n\tyhZSXOzxXzHWHKhJEGAtSJyYMDBP063Jz9UTjhaiNQ=="
     , Rfc2822.OptionalField
         "X-Google-Smtp-Source"
         " AAOMgpeSPyQt6gLoYRDkyjOpo2rN1aBvzv1SdKcmo5DCACWDqPLCUXyocBsaxfYTTsKYRkJp2O9D67xiJMxr5asnlF0="
     , Rfc2822.OptionalField
         "X-Received"
         " by 2002:a37:1028:: with SMTP id a40-v6mr21175009qkh.257.1531216941632;\r\n Tue, 10 Jul 2018 03:02:21 -0700 (PDT)"
     , Rfc2822.OptionalField "MIME-Version" " 1.0"
     , Rfc2822.References
         [ "<CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubWTWSg@mail.gmail.com>"
         ]
     , Rfc2822.InReplyTo
         [ "<CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubWTWSg@mail.gmail.com>"
         ]
     , Rfc2822.ReplyTo
         [ Rfc2822.NameAddr
             { Rfc2822.nameAddr_name = Nothing
             , Rfc2822.nameAddr_addr = "chrisdone@googlemail.com"
             }
         ]
     , Rfc2822.From
         [ Rfc2822.NameAddr
             { Rfc2822.nameAddr_name = Just "Christopher Done"
             , Rfc2822.nameAddr_addr = "chrisdone@gmail.com"
             }
         ]
     , Rfc2822.Date
         (CalendarTime
            { ctYear = 2018
            , ctMonth = July
            , ctDay = 10
            , ctHour = 11
            , ctMin = 2
            , ctSec = 10
            , ctPicosec = 0
            , ctWDay = Tuesday
            , ctYDay = 0
            , ctTZName = ""
            , ctTZ = 3600
            , ctIsDST = False
            })
     , Rfc2822.MessageID
         "<CAAJHNPCnR2LVyN+Ns5TauNTC9Gb1hVnUHGD8+fKstAqm_5yvQA@mail.gmail.com>"
     , Rfc2822.Subject " Re: wibbling"
     , Rfc2822.To
         [ Rfc2822.NameAddr
             { Rfc2822.nameAddr_name = Nothing
             , Rfc2822.nameAddr_addr = "wibble@chrisdone.com"
             }
         ]
     , Rfc2822.OptionalField
         "Content-Type"
         " multipart/alternative; boundary=\"000000000000e1518b0570a23972\""
     ]
     "--000000000000e1518b0570a23972\r\nContent-Type: text/plain; charset=\"UTF-8\"\r\n\r\nwe continue!\r\n\r\nOn Tue, 10 Jul 2018 at 10:36, Christopher Done <chrisdone@gmail.com> wrote:\r\n\r\n> hey wibble\r\n>\r\n\r\n--000000000000e1518b0570a23972\r\nContent-Type: text/html; charset=\"UTF-8\"\r\nContent-Transfer-Encoding: quoted-printable\r\n\r\n<div dir=3D\"ltr\">we continue!</div><br><div class=3D\"gmail_quote\"><div dir=\r\n=3D\"ltr\">On Tue, 10 Jul 2018 at 10:36, Christopher Done &lt;<a href=3D\"mail=\r\nto:chrisdone@gmail.com\">chrisdone@gmail.com</a>&gt; wrote:<br></div><blockq=\r\nuote class=3D\"gmail_quote\" style=3D\"margin:0 0 0 .8ex;border-left:1px #ccc =\r\nsolid;padding-left:1ex\"><div dir=3D\"ltr\">hey wibble</div>\r\n</blockquote></div>\r\n\r\n--000000000000e1518b0570a23972--\r\n.\r\n")

postfixMessageParsed :: Rfc2822.GenericMessage ByteString
postfixMessageParsed =
  Rfc2822.Message
    [ Rfc2822.Received
        ( [ ("from", "solution.localdomain")
          , ("by", "somemore.net")
          , ("with", "ESMTPSA")
          , ("id", "E796969A3235")
          , ("for", "blah@chrisdone.com")
          ]
        , CalendarTime
            { ctYear = 2018
            , ctMonth = July
            , ctDay = 9
            , ctHour = 20
            , ctMin = 48
            , ctSec = 0
            , ctPicosec = 0
            , ctWDay = Monday
            , ctYDay = 0
            , ctTZName = ""
            , ctTZ = 10800
            , ctIsDST = False
            })
    , Rfc2822.Received
        ( [ ("from", "[127.0.0.1]")
          , ("by", "solution.localdomain")
          , ("with", "ESMTP")
          , ("id", "0D482240326")
          , ("for", "blah@chrisdone.com")
          ]
        , CalendarTime
            { ctYear = 2018
            , ctMonth = July
            , ctDay = 9
            , ctHour = 20
            , ctMin = 48
            , ctSec = 0
            , ctPicosec = 0
            , ctWDay = Monday
            , ctYDay = 0
            , ctTZName = ""
            , ctTZ = 10800
            , ctIsDST = False
            })
    , Rfc2822.To
        [ Rfc2822.NameAddr
            { Rfc2822.nameAddr_name = Nothing
            , Rfc2822.nameAddr_addr = "blah@chrisdone.com"
            }
        ]
    , Rfc2822.From
        [ Rfc2822.NameAddr
            { Rfc2822.nameAddr_name = Just "Mihai Bazon"
            , Rfc2822.nameAddr_addr = "mihai@bazon.net"
            }
        ]
    , Rfc2822.Subject " test"
    , Rfc2822.MessageID "<2c954fd9-7216-4ed5-f303-69b4e811821d@bazon.net>"
    , Rfc2822.Date
        (CalendarTime
           { ctYear = 2018
           , ctMonth = July
           , ctDay = 9
           , ctHour = 20
           , ctMin = 47
           , ctSec = 59
           , ctPicosec = 0
           , ctWDay = Monday
           , ctYDay = 0
           , ctTZName = ""
           , ctTZ = 10800
           , ctIsDST = False
           })
    , Rfc2822.OptionalField
        "User-Agent"
        " Mozilla/5.0 (X11; Linux x86_64; rv:52.0) Gecko/20100101\r\n Thunderbird/52.8.0"
    , Rfc2822.OptionalField "MIME-Version" " 1.0"
    , Rfc2822.OptionalField
        "Content-Type"
        " text/plain; charset=utf-8; format=flowed"
    , Rfc2822.OptionalField "Content-Transfer-Encoding" " 7bit"
    , Rfc2822.OptionalField "Content-Language" " en-US"
    ]
    "blah\r\n\r\n.\r\n"

gmailMessage :: ByteString
gmailMessage =
  "Received: by mail-qk0-f170.google.com with SMTP id b66-v6so11206427qkj.1\r\n \
  \       for <wibble@chrisdone.com>; Tue, 10 Jul 2018 03:02:22 -0700 (PDT)\r\nD\
  \KIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=gmail.com; \
  \s=20161025;\r\n        h=mime-version:references:in-reply-to:reply-to:from:da\
  \te:message-id\r\n         :subject:to;\r\n        bh=vuaco1M4EZ4dKC+65I2ne6a/\
  \89CLSI3xS3oRZH/qv5s=;\r\n        b=NL2PC7xTlI2ampgdRB9B6WzMj/bP+mOvlw/Rtd2+27\
  \EMSg5eRdJbs7LjSz8GJV22pl\r\n         hF9C8DLTKRo9BrrE6qs27oPMCG0+/eXwOgBQsw/T\
  \R2yjvDT2oBnBRkfVjqakVBhmg2GT\r\n         Ro+iGjDx8vC1136fI/7A3iXNJnjLHAIYcoMf\
  \HEljL7s4hqX8jHzQeWG6+W9jLLH08DFS\r\n         IutcEUAnM1DKi8gPP69Qm6i8mEKfHwb8\
  \tals8RRthMUhu1w1Hey3djEB5SWpOhU+01BT\r\n         fcGxYD10K5ED+T3FfX6CPC+4PMt/\
  \7va2ZD8XEfD2Hiogn1unuhjz++jArvq6jWxIxZmB\r\n         eQrg==\r\nX-Google-DKIM-\
  \Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=1e100.net; s=20\
  \161025;\r\n        h=x-gm-message-state:mime-version:references:in-reply-to:r\
  \eply-to\r\n         :from:date:message-id:subject:to;\r\n        bh=vuaco1M4E\
  \Z4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=;\r\n        b=soHCjg9EPG07DTHqu/JJyV4+HDq\
  \PVNFuLfE56qtggay+5RheihifZ26n/B6CRUSV0x\r\n         5C9IxFH1FSaPjkEugcW3wfE39\
  \zZyUqro+Ozj1IiB0Za0Srw55Iu5YaYyeEBkSY+VVpdY\r\n         NlkNg0/m+gfGo7B05f6Cj\
  \zqbPQ5iwmOx5kAwS4jKARef0K6Dv2HbeRHYqVCNt66sPGED\r\n         zE6TFPxpbdkrDdSdy\
  \tTevnu00e8IrqvC9rEft6r/nSrP2i0NpdUEuDa+LCFBWfGWwsuV\r\n         hrVAtkCTfPOGg\
  \JpFCZ+b8uWWqovhnkfWrx0TE0uksW64YqdJXIH9VXGhnWRletjnz6+C\r\n         AAyg=\
  \=\r\nX-Gm-Message-State: APt69E11MY8bREVIjYEX2S7HZECtZlfMQ8zqcH1I2F0mDl6fWrrR\
  \3grG\r\n\tyhZSXOzxXzHWHKhJEGAtSJyYMDBP063Jz9UTjhaiNQ==\r\nX-Google-Smtp-Sourc\
  \e: AAOMgpeSPyQt6gLoYRDkyjOpo2rN1aBvzv1SdKcmo5DCACWDqPLCUXyocBsaxfYTTsKYRkJp2O\
  \9D67xiJMxr5asnlF0=\r\nX-Received: by 2002:a37:1028:: with SMTP id a40-v6mr211\
  \75009qkh.257.1531216941632;\r\n Tue, 10 Jul 2018 03:02:21 -0700 (PDT)\r\nMIME\
  \-Version: 1.0\r\nReferences: <CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubW\
  \TWSg@mail.gmail.com>\r\nIn-Reply-To: <CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7\
  \p1AUYubWTWSg@mail.gmail.com>\r\nReply-To: chrisdone@googlemail.com\r\nFrom: C\
  \hristopher Done <chrisdone@gmail.com>\r\nDate: Tue, 10 Jul 2018 11:02:10 +010\
  \0\r\nMessage-ID: <CAAJHNPCnR2LVyN+Ns5TauNTC9Gb1hVnUHGD8+fKstAqm_5yvQA@mail.gm\
  \ail.com>\r\nSubject: Re: wibbling\r\nTo: wibble@chrisdone.com\r\nContent-Type\
  \: multipart/alternative; boundary=\"000000000000e1518b0570a23972\"\r\n\r\n--0\
  \00000000000e1518b0570a23972\r\nContent-Type: text/plain; charset=\"UTF-\
  \8\"\r\n\r\nwe continue!\r\n\r\nOn Tue, 10 Jul 2018 at 10:36, Christopher Done\
  \ <chrisdone@gmail.com> wrote:\r\n\r\n> hey wibble\r\n>\r\n\r\n--000000000000e\
  \1518b0570a23972\r\nContent-Type: text/html; charset=\"UTF-8\"\r\nContent-Tran\
  \sfer-Encoding: quoted-printable\r\n\r\n<div dir=3D\"ltr\">we continue!</div><\
  \br><div class=3D\"gmail_quote\"><div dir=\r\n=3D\"ltr\">On Tue, 10 Jul 2018 a\
  \t 10:36, Christopher Done &lt;<a href=3D\"mail=\r\nto:chrisdone@gmail.com\">c\
  \hrisdone@gmail.com</a>&gt; wrote:<br></div><blockq=\r\nuote class=3D\"gmail_q\
  \uote\" style=3D\"margin:0 0 0 .8ex;border-left:1px #ccc =\r\nsolid;padding-le\
  \ft:1ex\"><div dir=3D\"ltr\">hey wibble</div>\r\n</blockquote></div>\r\n\r\n--\
  \000000000000e1518b0570a23972--\r\n.\r\n"

postfixMessage :: ByteString
postfixMessage =
  "Received: from solution.localdomain (unknown [79.112.116.6])\r\n\tby somemore\
               \.net (Postfix) with ESMTPSA id E796969A3235\r\n\tfor <blah@chris\
  \done.com>; Mon,  9 Jul 2018 20:48:00 +0300 (EEST)\r\nReceived: from [127.0.0.\
  \1] (localhost.localdomain [127.0.0.1])\r\n\tby solution.localdomain (Postfix)\
  \ with ESMTP id 0D482240326\r\n\tfor <blah@chrisdone.com>; Mon,  9 Jul 2018 20\
  \:48:00 +0300 (EEST)\r\nTo: blah@chrisdone.com\r\nFrom: Mihai Bazon <mihai@baz\
  \on.net>\r\nSubject: test\r\nMessage-ID: <2c954fd9-7216-4ed5-f303-69b4e811821d\
  \@bazon.net>\r\nDate: Mon, 9 Jul 2018 20:47:59 +0300\r\nUser-Agent: Mozilla/5.\
  \0 (X11; Linux x86_64; rv:52.0) Gecko/20100101\r\n Thunderbird/52.8.0\r\nMIME-\
  \Version: 1.0\r\nContent-Type: text/plain; charset=utf-8; format=flowed\r\nCon\
  \tent-Transfer-Encoding: 7bit\r\nContent-Language: en-US\r\n\r\nblah\r\n\r\n.\r\n"

gmailMessageParsed2 :: EmailMessage
gmailMessageParsed2 =
  EmailMessage
    { emailHeaders =
        [ Header
            { headerName = "Received"
            , headerContents =
                "by mail-qk0-f170.google.com with SMTP id b66-v6so11206427qkj.1 for <wibble@chrisdone.com>; Tue, 10 Jul 2018 03:02:22 -0700 (PDT)"
            }
        , Header
            { headerName = "DKIM-Signature"
            , headerContents =
                "v=1; a=rsa-sha256; c=relaxed/relaxed; d=gmail.com; s=20161025; h=mime-version:references:in-reply-to:reply-to:from:date:message-id :subject:to; bh=vuaco1M4EZ4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=; b=NL2PC7xTlI2ampgdRB9B6WzMj/bP+mOvlw/Rtd2+27EMSg5eRdJbs7LjSz8GJV22pl hF9C8DLTKRo9BrrE6qs27oPMCG0+/eXwOgBQsw/TR2yjvDT2oBnBRkfVjqakVBhmg2GT Ro+iGjDx8vC1136fI/7A3iXNJnjLHAIYcoMfHEljL7s4hqX8jHzQeWG6+W9jLLH08DFS IutcEUAnM1DKi8gPP69Qm6i8mEKfHwb8tals8RRthMUhu1w1Hey3djEB5SWpOhU+01BT fcGxYD10K5ED+T3FfX6CPC+4PMt/7va2ZD8XEfD2Hiogn1unuhjz++jArvq6jWxIxZmB eQrg=="
            }
        , Header
            { headerName = "X-Google-DKIM-Signature"
            , headerContents =
                "v=1; a=rsa-sha256; c=relaxed/relaxed; d=1e100.net; s=20161025; h=x-gm-message-state:mime-version:references:in-reply-to:reply-to :from:date:message-id:subject:to; bh=vuaco1M4EZ4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=; b=soHCjg9EPG07DTHqu/JJyV4+HDqPVNFuLfE56qtggay+5RheihifZ26n/B6CRUSV0x 5C9IxFH1FSaPjkEugcW3wfE39zZyUqro+Ozj1IiB0Za0Srw55Iu5YaYyeEBkSY+VVpdY NlkNg0/m+gfGo7B05f6CjzqbPQ5iwmOx5kAwS4jKARef0K6Dv2HbeRHYqVCNt66sPGED zE6TFPxpbdkrDdSdytTevnu00e8IrqvC9rEft6r/nSrP2i0NpdUEuDa+LCFBWfGWwsuV hrVAtkCTfPOGgJpFCZ+b8uWWqovhnkfWrx0TE0uksW64YqdJXIH9VXGhnWRletjnz6+C AAyg=="
            }
        , Header
            { headerName = "X-Gm-Message-State"
            , headerContents =
                "APt69E11MY8bREVIjYEX2S7HZECtZlfMQ8zqcH1I2F0mDl6fWrrR3grG yhZSXOzxXzHWHKhJEGAtSJyYMDBP063Jz9UTjhaiNQ=="
            }
        , Header
            { headerName = "X-Google-Smtp-Source"
            , headerContents =
                "AAOMgpeSPyQt6gLoYRDkyjOpo2rN1aBvzv1SdKcmo5DCACWDqPLCUXyocBsaxfYTTsKYRkJp2O9D67xiJMxr5asnlF0="
            }
        , Header
            { headerName = "X-Received"
            , headerContents =
                "by 2002:a37:1028:: with SMTP id a40-v6mr21175009qkh.257.1531216941632; Tue, 10 Jul 2018 03:02:21 -0700 (PDT)"
            }
        , Header {headerName = "MIME-Version", headerContents = "1.0"}
        , References
            [ "CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubWTWSg@mail.gmail.com"
            ]
        , InReplyTo
            "CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubWTWSg@mail.gmail.com"
        , ReplyTo
            (EmailAddress
               {emailAddress = "chrisdone@googlemail.com", emailLabel = Nothing})
        , From
            (EmailAddress
               { emailAddress = "chrisdone@gmail.com"
               , emailLabel = Just "Christopher Done"
               })
        , Date (ZonedTime (LocalTime (fromGregorian 2018 07 10)
                                     (TimeOfDay 11 02 10))
                          (hoursToTimeZone 1))
        , MessageId
            "CAAJHNPCnR2LVyN+Ns5TauNTC9Gb1hVnUHGD8+fKstAqm_5yvQA@mail.gmail.com"
        , Subject "Re: wibbling"
        , To
            [ EmailAddress
                {emailAddress = "wibble@chrisdone.com", emailLabel = Nothing}
            ]
        , Header
            { headerName = "Content-Type"
            , headerContents =
                "multipart/alternative; boundary=\"000000000000e1518b0570a23972\""
            }
        ]
    , emailBodies =
        [ MessageBody
            (EmailMessage
               { emailHeaders =
                   [ Header
                       { headerName = "Content-Type"
                       , headerContents = "text/plain; charset=\"UTF-8\""
                       }
                   ]
               , emailBodies =
                   [ TextBody
                       "we continue!\r\n\r\nOn Tue, 10 Jul 2018 at 10:36, Christopher Done <chrisdone@gmail.com> wrote:\r\n\r\n> hey wibble\r\n>\r\n\r\n"
                   ]
               })
        , MessageBody
            (EmailMessage
               { emailHeaders =
                   [ Header
                       { headerName = "Content-Type"
                       , headerContents = "text/html; charset=\"UTF-8\""
                       }
                   , Header
                       { headerName = "Content-Transfer-Encoding"
                       , headerContents = "quoted-printable"
                       }
                   ]
               , emailBodies =
                   [ TextBody
                       "<div dir=\"ltr\">we continue!</div><br><div class=\"gmail_quote\"><div dir=\"ltr\">On Tue, 10 Jul 2018 at 10:36, Christopher Done &lt;<a href=\"mailto:chrisdone@gmail.com\">chrisdone@gmail.com</a>&gt; wrote:<br></div><blockquote class=\"gmail_quote\" style=\"margin:0 0 0 .8ex;border-left:1px #ccc solid;padding-left:1ex\"><div dir=\"ltr\">hey wibble</div>\r\n</blockquote></div>\r\n\r\n"
                   ]
               })
        ]
    }

postfixMessageParsed2 :: EmailMessage
postfixMessageParsed2 =
  EmailMessage
    { emailHeaders =
        [ Header
            { headerName = "Received"
            , headerContents =
                "from solution.localdomain (unknown [79.112.116.6]) by somemore.net (Postfix) with ESMTPSA id E796969A3235 for <blah@chrisdone.com>; Mon,  9 Jul 2018 20:48:00 +0300 (EEST)"
            }
        , Header
            { headerName = "Received"
            , headerContents =
                "from [127.0.0.1] (localhost.localdomain [127.0.0.1]) by solution.localdomain (Postfix) with ESMTP id 0D482240326 for <blah@chrisdone.com>; Mon,  9 Jul 2018 20:48:00 +0300 (EEST)"
            }
        , To
            [ EmailAddress
                {emailAddress = "blah@chrisdone.com", emailLabel = Nothing}
            ]
        , From
            (EmailAddress
               { emailAddress = "mihai@bazon.net"
               , emailLabel = Just "Mihai Bazon"
               })
        , Subject "test"
        , MessageId "2c954fd9-7216-4ed5-f303-69b4e811821d@bazon.net"
        , Date
            ((ZonedTime
                (LocalTime (fromGregorian 2018 07 09) (TimeOfDay 20 47 59))
                (hoursToTimeZone 3)))
        , Header
            { headerName = "User-Agent"
            , headerContents =
                "Mozilla/5.0 (X11; Linux x86_64; rv:52.0) Gecko/20100101 Thunderbird/52.8.0"
            }
        , Header {headerName = "MIME-Version", headerContents = "1.0"}
        , Header
            { headerName = "Content-Type"
            , headerContents = "text/plain; charset=utf-8; format=flowed"
            }
        , Header
            { headerName = "Content-Transfer-Encoding"
            , headerContents = "7bit"
            }
        , Header {headerName = "Content-Language", headerContents = "en-US"}
        ]
    , emailBodies = [TextBody "blah\r\n\r\n.\r\n"]
    }
