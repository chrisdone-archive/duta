{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Time
import           Database.Persist.Sqlite
import           Duta.Model
import qualified Duta.SMTP.Receiver
import           Duta.SMTP.Receiver.MIME
import           Duta.Types.MIME
import qualified Duta.Types.Model
import           System.Time
import           Test.Hspec
import qualified Text.Parsec as Parsec
import           Text.Parsec.Rfc2822 as Rfc2822

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "Integration"
    (do integrationRegression
        writingToDb)
  describe
    "RFC 2822 Parser"
    (do it
          "GMail example"
          (shouldBe
             (fmap
                parseMessageBodyTree
                (Parsec.parse Rfc2822.message "" gmailMessage))
             (Right gmailMessageParsed))
        it
          "Postfix example"
          (shouldBe
             (fmap
                parseMessageBodyTree
                (Parsec.parse Rfc2822.message "" postfixMessage))
             (Right postfixMessageParsed))
        it
          "GMail Attachment example"
          (shouldBe
             (fmap
                parseMessageBodyTree
                (Parsec.parse Rfc2822.message "" gmailAttachmentMessage))
             (Right gmailAttachmentParsed)))

writingToDb :: Spec
writingToDb =
  describe
    "Writing to DB"
    (it
       "GMail"
       (do now <- getCurrentTime
           let message' =
                 Duta.Types.Model.Message
                   { Duta.Types.Model.messageReceived = now
                   , Duta.Types.Model.messageAuthored =
                       read "2018-07-10 11:02:10 UTC"
                   , Duta.Types.Model.messageFrom = "chrisdone@gmail.com"
                   , Duta.Types.Model.messageTo = "wibble@chrisdone.com"
                   , Duta.Types.Model.messageSubject = " Re: wibbling"
                   }
           (replies, inserted) <-
             runNoLoggingT
               (withSqliteConnInfo
                  (mkSqliteConnectionInfo ":memory:")
                  (runReaderT
                     (do _ <- runMigration Duta.Types.Model.migrateAll
                         replies <-
                           C.runConduit
                             (CL.sourceList gmailInput .|
                              Duta.SMTP.Receiver.interaction
                                Duta.SMTP.Receiver.Interaction
                                  { Duta.SMTP.Receiver.interactionHostname = ""
                                  , Duta.SMTP.Receiver.interactionOnMessage =
                                      insertModelMessage now
                                  , Duta.SMTP.Receiver.interactionReply =
                                      C.yield
                                  } .|
                              CL.consume)
                         inserted <- selectList [] []
                         pure (replies, inserted))))
           shouldBe inserted [Entity (toSqlKey 1) message']
           shouldBe
             replies
             [ Duta.SMTP.Receiver.ServiceReady ""
             , Duta.SMTP.Receiver.Okay " OK"
             , Duta.SMTP.Receiver.Okay " OK"
             , Duta.SMTP.Receiver.Okay " OK"
             , Duta.SMTP.Receiver.StartMailInput
             , Duta.SMTP.Receiver.Okay " OK"
             , Duta.SMTP.Receiver.Closing
             ]))

integrationRegression :: Spec
integrationRegression = do
  it
    "Regression test against GMail"
    (do xs <-
          runNoLoggingT
            (C.runConduit $
             CL.sourceList gmailInput .|
             Duta.SMTP.Receiver.interaction
               Duta.SMTP.Receiver.Interaction
                 { Duta.SMTP.Receiver.interactionHostname = ""
                 , Duta.SMTP.Receiver.interactionOnMessage = const (pure ())
                 , Duta.SMTP.Receiver.interactionReply = C.yield
                 } .|
             CL.consume)
        shouldBe
          xs
          [ Duta.SMTP.Receiver.ServiceReady ""
          , Duta.SMTP.Receiver.Okay " OK"
          , Duta.SMTP.Receiver.Okay " OK"
          , Duta.SMTP.Receiver.Okay " OK"
          , Duta.SMTP.Receiver.StartMailInput
          , Duta.SMTP.Receiver.Okay " OK"
          , Duta.SMTP.Receiver.Closing
          ])
  it
    "Regression test against Postfix"
    (do xs <-
          runNoLoggingT $
          C.runConduit $
          CL.sourceList postfixInput .|
          Duta.SMTP.Receiver.interaction
            Duta.SMTP.Receiver.Interaction
              { Duta.SMTP.Receiver.interactionHostname = ""
              , Duta.SMTP.Receiver.interactionOnMessage = const (pure ())
              , Duta.SMTP.Receiver.interactionReply = C.yield
              } .|
          CL.consume
        shouldBe
          xs
          [ Duta.SMTP.Receiver.ServiceReady ""
          , Duta.SMTP.Receiver.Okay " OK"
          , Duta.SMTP.Receiver.Okay " OK"
          , Duta.SMTP.Receiver.Okay " OK"
          , Duta.SMTP.Receiver.StartMailInput
          , Duta.SMTP.Receiver.Okay " OK"
          , Duta.SMTP.Receiver.Closing
          ])

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

gmailMessageParsed :: GenericMessage (BodyTree (Rfc2822.GenericMessage ByteString))
gmailMessageParsed =
  (Message
     [ Received
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
     , OptionalField
         "DKIM-Signature"
         " v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=gmail.com; s=20161025;\r\n        h=mime-version:references:in-reply-to:reply-to:from:date:message-id\r\n         :subject:to;\r\n        bh=vuaco1M4EZ4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=;\r\n        b=NL2PC7xTlI2ampgdRB9B6WzMj/bP+mOvlw/Rtd2+27EMSg5eRdJbs7LjSz8GJV22pl\r\n         hF9C8DLTKRo9BrrE6qs27oPMCG0+/eXwOgBQsw/TR2yjvDT2oBnBRkfVjqakVBhmg2GT\r\n         Ro+iGjDx8vC1136fI/7A3iXNJnjLHAIYcoMfHEljL7s4hqX8jHzQeWG6+W9jLLH08DFS\r\n         IutcEUAnM1DKi8gPP69Qm6i8mEKfHwb8tals8RRthMUhu1w1Hey3djEB5SWpOhU+01BT\r\n         fcGxYD10K5ED+T3FfX6CPC+4PMt/7va2ZD8XEfD2Hiogn1unuhjz++jArvq6jWxIxZmB\r\n         eQrg=="
     , OptionalField
         "X-Google-DKIM-Signature"
         " v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=1e100.net; s=20161025;\r\n        h=x-gm-message-state:mime-version:references:in-reply-to:reply-to\r\n         :from:date:message-id:subject:to;\r\n        bh=vuaco1M4EZ4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=;\r\n        b=soHCjg9EPG07DTHqu/JJyV4+HDqPVNFuLfE56qtggay+5RheihifZ26n/B6CRUSV0x\r\n         5C9IxFH1FSaPjkEugcW3wfE39zZyUqro+Ozj1IiB0Za0Srw55Iu5YaYyeEBkSY+VVpdY\r\n         NlkNg0/m+gfGo7B05f6CjzqbPQ5iwmOx5kAwS4jKARef0K6Dv2HbeRHYqVCNt66sPGED\r\n         zE6TFPxpbdkrDdSdytTevnu00e8IrqvC9rEft6r/nSrP2i0NpdUEuDa+LCFBWfGWwsuV\r\n         hrVAtkCTfPOGgJpFCZ+b8uWWqovhnkfWrx0TE0uksW64YqdJXIH9VXGhnWRletjnz6+C\r\n         AAyg=="
     , OptionalField
         "X-Gm-Message-State"
         " APt69E11MY8bREVIjYEX2S7HZECtZlfMQ8zqcH1I2F0mDl6fWrrR3grG\r\n\tyhZSXOzxXzHWHKhJEGAtSJyYMDBP063Jz9UTjhaiNQ=="
     , OptionalField
         "X-Google-Smtp-Source"
         " AAOMgpeSPyQt6gLoYRDkyjOpo2rN1aBvzv1SdKcmo5DCACWDqPLCUXyocBsaxfYTTsKYRkJp2O9D67xiJMxr5asnlF0="
     , OptionalField
         "X-Received"
         " by 2002:a37:1028:: with SMTP id a40-v6mr21175009qkh.257.1531216941632;\r\n Tue, 10 Jul 2018 03:02:21 -0700 (PDT)"
     , OptionalField "MIME-Version" " 1.0"
     , References
         [ "<CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubWTWSg@mail.gmail.com>"
         ]
     , InReplyTo
         [ "<CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubWTWSg@mail.gmail.com>"
         ]
     , ReplyTo
         [ NameAddr
             { nameAddr_name = Nothing
             , nameAddr_addr = "chrisdone@googlemail.com"
             }
         ]
     , From
         [ NameAddr
             { nameAddr_name = Just "Christopher Done"
             , nameAddr_addr = "chrisdone@gmail.com"
             }
         ]
     , Date
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
     , MessageID
         "<CAAJHNPCnR2LVyN+Ns5TauNTC9Gb1hVnUHGD8+fKstAqm_5yvQA@mail.gmail.com>"
     , Subject " Re: wibbling"
     , To
         [ NameAddr
             {nameAddr_name = Nothing, nameAddr_addr = "wibble@chrisdone.com"}
         ]
     , OptionalField
         "Content-Type"
         " multipart/alternative; boundary=\"000000000000e1518b0570a23972\""
     ]
     (BodyMultipart
        [ Right
            (BodyPart
               (Message
                  [ OptionalField
                      "Content-Type"
                      " text/plain; charset=\"UTF-8\""
                  ]
                  "we continue!\r\n\r\nOn Tue, 10 Jul 2018 at 10:36, Christopher Done <chrisdone@gmail.com> wrote:\r\n\r\n> hey wibble\r\n>\r\n\r\n"))
        , Right
            (BodyPart
               (Message
                  [ OptionalField "Content-Type" " text/html; charset=\"UTF-8\""
                  , OptionalField
                      "Content-Transfer-Encoding"
                      " quoted-printable"
                  ]
                  "<div dir=3D\"ltr\">we continue!</div><br><div class=3D\"gmail_quote\"><div dir=\r\n=3D\"ltr\">On Tue, 10 Jul 2018 at 10:36, Christopher Done &lt;<a href=3D\"mail=\r\nto:chrisdone@gmail.com\">chrisdone@gmail.com</a>&gt; wrote:<br></div><blockq=\r\nuote class=3D\"gmail_quote\" style=3D\"margin:0 0 0 .8ex;border-left:1px #ccc =\r\nsolid;padding-left:1ex\"><div dir=3D\"ltr\">hey wibble</div>\r\n</blockquote></div>\r\n\r\n"))
        ]))

postfixMessageParsed :: GenericMessage (BodyTree (Rfc2822.GenericMessage ByteString))
postfixMessageParsed =
  (Message
     [ Received
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
     , Received
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
     , To
         [ NameAddr
             {nameAddr_name = Nothing, nameAddr_addr = "blah@chrisdone.com"}
         ]
     , From
         [ NameAddr
             { nameAddr_name = Just "Mihai Bazon"
             , nameAddr_addr = "mihai@bazon.net"
             }
         ]
     , Subject " test"
     , MessageID "<2c954fd9-7216-4ed5-f303-69b4e811821d@bazon.net>"
     , Date
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
     , OptionalField
         "User-Agent"
         " Mozilla/5.0 (X11; Linux x86_64; rv:52.0) Gecko/20100101\r\n Thunderbird/52.8.0"
     , OptionalField "MIME-Version" " 1.0"
     , OptionalField "Content-Type" " text/plain; charset=utf-8; format=flowed"
     , OptionalField "Content-Transfer-Encoding" " 7bit"
     , OptionalField "Content-Language" " en-US"
     ]
     (BodyPart
        (Message
           [ OptionalField
               "User-Agent"
               " Mozilla/5.0 (X11; Linux x86_64; rv:52.0) Gecko/20100101\r\n Thunderbird/52.8.0"
           , OptionalField "MIME-Version" " 1.0"
           , OptionalField
               "Content-Type"
               " text/plain; charset=utf-8; format=flowed"
           , OptionalField "Content-Transfer-Encoding" " 7bit"
           , OptionalField "Content-Language" " en-US"
           ]
           "blah\r\n\r\n.\r\n")))

gmailAttachmentParsed :: GenericMessage (BodyTree (Rfc2822.GenericMessage ByteString))
gmailAttachmentParsed =
  Message
    [ Received
        ( [ ("by", "mail-qt0-f171.google.com")
          , ("with", "SMTP")
          , ("id", "e19-v6so7264767qtp.8")
          , ("for", "gmail@chrisdone.com")
          ]
        , CalendarTime
            { ctYear = 2018
            , ctMonth = July
            , ctDay = 19
            , ctHour = 7
            , ctMin = 9
            , ctSec = 3
            , ctPicosec = 0
            , ctWDay = Thursday
            , ctYDay = 0
            , ctTZName = ""
            , ctTZ = -25200
            , ctIsDST = False
            })
    , OptionalField
        "X-Google-DKIM-Signature"
        " v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=1e100.net; s=20161025;\r\n        h=x-gm-message-state:delivered-to:dkim-signature:mime-version\r\n         :reply-to:from:date:message-id:subject:to;\r\n        bh=Oli/DfRXOO3EH3Cpo48noh3QdZQ2z2xbdYBhVxjaTOI=;\r\n        b=qH82tD41/S0PazkpbjZnRtbusDRWavXYD/rlhvITJAqniJ9egT1bZPotIwd0vEWV+R\r\n         ns1IJD/nkB0pIYscDPC0SHlgG5NSf7eT4/N9FXM1nBDOSTEoXluWmEDDNLrA+I/v8LNf\r\n         E/QpR4RObmTh9wWjGc/Ybd4ZWXY+BA4iUteTr8XYuup3/vBbT+SJuYYPEgFRdCaXP0mn\r\n         sgi3cuChqyZsmbdTZ03A+Dk0qH0VFdp28e+61t1eDdLMDPiNzbS2hw6wlJIpKmY6Gvh0\r\n         eXYwvShOFq2NmbOkXCwxglp7WzlHD+3b93zsgHtRSQWGdz1ESR3c2FtlSg44lYmtG8Ys\r\n         lGMg=="
    , OptionalField
        "X-Gm-Message-State"
        " AOUpUlEvxWR7XP0aUlTk7Gjivd1eWOPTNPmdMbdHSK9qBGs8rFEb50Sd\r\n\t4LzEFkdsMUBwIuOjwdeYH+3yAu/uxstOk9fvBRCPYP1liQL7EAbcCg=="
    , OptionalField
        "X-Received"
        " by 2002:ac8:246b:: with SMTP id d40-v6mr9987658qtd.196.1532009343604;\r\n        Thu, 19 Jul 2018 07:09:03 -0700 (PDT)"
    , OptionalField "X-Forwarded-To" " gmail@chrisdone.com"
    , OptionalField "X-Forwarded-For" " chrisdone@gmail.com gmail@chrisdone.com"
    , OptionalField "Delivered-To" " chrisdone@gmail.com"
    , OptionalField
        "Received"
        " by 2002:ac8:363:0:0:0:0:0 with SMTP id w35-v6csp1775618qtg;\r\n        Thu, 19 Jul 2018 07:09:02 -0700 (PDT)"
    , OptionalField
        "X-Received"
        " by 2002:a0c:9692:: with SMTP id a18-v6mr10975278qvd.16.1532009342920;\r\n        Thu, 19 Jul 2018 07:09:02 -0700 (PDT)"
    , OptionalField
        "ARC-Seal"
        " i=1; a=rsa-sha256; t=1532009342; cv=none;\r\n        d=google.com; s=arc-20160816;\r\n        b=uuOieQD4b/ilUKSb9Nt1yk6S+p2b3KSaT2y8mHsbT68iEDWYlWggx+7UVofi3UmLWm\r\n         Xb59gox1168w4A4C3tJyd/5F8/IWWf3ICgkespW76LRz4RFLrOtmSSX8KqXjbqhQgxVO\r\n         Zn7oaBeEyLVAwPXMbCl5BEJMNnDDyChCrFNDtkIv2zKowqQ304iLUOaTUIpryjzuG4KW\r\n         iyc9ogiISsFGMEsxrCQiEZx4L2IdVMldeejGGL5j4oCX0HMs9PLSk3wbtVMF+quywobm\r\n         pdYsmlUq5+9DDDnespiJNyhzm1niNVOPEWT/t3MyHc2WxlzGYWrWy1BNRy1z3V4bkyLa\r\n         ue6A=="
    , OptionalField
        "ARC-Message-Signature"
        " i=1; a=rsa-sha256; c=relaxed/relaxed; d=google.com; s=arc-20160816;\r\n        h=to:subject:message-id:date:from:reply-to:mime-version\r\n         :dkim-signature:arc-authentication-results;\r\n        bh=Oli/DfRXOO3EH3Cpo48noh3QdZQ2z2xbdYBhVxjaTOI=;\r\n        b=agS65Xltmi+hxKgmxjiMGtziCdsSW0EMp3HcYcKzk0TqHJIos2jIwWQ1m1lV2g0FzF\r\n         +Zihia+r5+w35pjzv7/caV3q7NLy92hf5DJEIUQawduL4Kf4A9eB8ui7sikOxr3XzJD3\r\n         MM0kevphd19djEeuRRBOI91hd909mXQv1Xu8yKzPECa0H4vo1CJt/lDltRTtf9fHsS2o\r\n         qlczFBBQBWSb311/PTPwZxDeTR5hxHp0sCKwrz44hV7h/5onPXPKBrmi82x3KDJyBrAR\r\n         9nLFAt2FbW2zte3kuXszNLU5F7qJjvBnzGl8Eu4eITAdLK3iHyyV7wn5ldH6fJpYdbNX\r\n         G86A=="
    , OptionalField
        "ARC-Authentication-Results"
        " i=1; mx.google.com;\r\n       dkim=pass header.i=@gmail.com header.s=20161025 header.b=EvyvF+GU;\r\n       spf=pass (google.com: domain of chrisdone@gmail.com designates 209.85.220.41 as permitted sender) smtp.mailfrom=chrisdone@gmail.com;\r\n       dmarc=pass (p=NONE sp=QUARANTINE dis=NONE) header.from=gmail.com"
    , ReturnPath "<chrisdone@gmail.com>"
    , Received
        ( [ ("from", "mail-sor-f41.google.com")
          , ("by", "mx.google.com")
          , ("with", "SMTPS")
          , ("id", "q86-v6sor2954059qkl.125.2018.07.19.07.09.02")
          , ("for", "chrisdone@gmail.com")
          ]
        , CalendarTime
            { ctYear = 2018
            , ctMonth = July
            , ctDay = 19
            , ctHour = 7
            , ctMin = 9
            , ctSec = 2
            , ctPicosec = 0
            , ctWDay = Thursday
            , ctYDay = 0
            , ctTZName = ""
            , ctTZ = -25200
            , ctIsDST = False
            })
    , OptionalField
        "Received-SPF"
        " pass (google.com: domain of chrisdone@gmail.com designates 209.85.220.41 as permitted sender) client-ip=209.85.220.41;"
    , OptionalField
        "Authentication-Results"
        " mx.google.com;\r\n       dkim=pass header.i=@gmail.com header.s=20161025 header.b=EvyvF+GU;\r\n       spf=pass (google.com: domain of chrisdone@gmail.com designates 209.85.220.41 as permitted sender) smtp.mailfrom=chrisdone@gmail.com;\r\n       dmarc=pass (p=NONE sp=QUARANTINE dis=NONE) header.from=gmail.com"
    , OptionalField
        "DKIM-Signature"
        " v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=gmail.com; s=20161025;\r\n        h=mime-version:reply-to:from:date:message-id:subject:to;\r\n        bh=Oli/DfRXOO3EH3Cpo48noh3QdZQ2z2xbdYBhVxjaTOI=;\r\n        b=EvyvF+GUxlJDAgAaVuVSXEmBQBw4SoRakKvtwH8/hlOmJr06WezNw7LnDc18zl9ipy\r\n         Ev6l6QxI5R7C0mNUab0MS5scVPd+6gLstFZLUDDeCoLQvWAJi87FE/NkjWGGKH8/HGwZ\r\n         8QkS1N/HN+ums0lOZj5qBRLVDnjxUZyIRXy3ll4w6kxwyGqnYEWaW5xw6ZsM4o5pMoPc\r\n         Rdsr7tf2FJ6PTeZn2c4vUZ7qSluZ4fh6XEq3GKKgX3pCrsUnGHkOWB6fUeJ2WSP7Mtwu\r\n         4SP9hvoMLJ5RXP45iOKxNXVMPnDf4F45XkfltRnfB6NPqt1Lt9lRMu8Dq1fLaMn1P9gi\r\n         cdSg=="
    , OptionalField
        "X-Google-Smtp-Source"
        " AAOMgpeIG6F5LUaxSpQAVmrSnpyHQbIwtKXJ8Pq9iMYg2hNzDXJD3T6HnaubNg2jziTurWge+pMNfm5Mqu0AgZUo8+E="
    , OptionalField
        "X-Received"
        " by 2002:a37:b506:: with SMTP id e6-v6mr9121547qkf.255.1532009342201;\r\n Thu, 19 Jul 2018 07:09:02 -0700 (PDT)"
    , OptionalField "MIME-Version" " 1.0"
    , ReplyTo
        [ NameAddr
            { nameAddr_name = Nothing
            , nameAddr_addr = "chrisdone@googlemail.com"
            }
        ]
    , From
        [ NameAddr
            { nameAddr_name = Just "Christopher Done"
            , nameAddr_addr = "chrisdone@gmail.com"
            }
        ]
    , Date
        (CalendarTime
           { ctYear = 2018
           , ctMonth = July
           , ctDay = 19
           , ctHour = 15
           , ctMin = 8
           , ctSec = 50
           , ctPicosec = 0
           , ctWDay = Thursday
           , ctYDay = 0
           , ctTZName = ""
           , ctTZ = 3600
           , ctIsDST = False
           })
    , MessageID
        "<CAAJHNPC_aoWp0AFqHtr2cfYLsC=uWnQ_iMUNn-hcOdX-SDC-Qg@mail.gmail.com>"
    , Subject " Smaller file"
    , To
        [ NameAddr
            { nameAddr_name = Just "Chris D"
            , nameAddr_addr = "chrisdone@gmail.com"
            }
        ]
    , OptionalField
        "Content-Type"
        " multipart/mixed; boundary=\"000000000000a2a85205715ab8f1\""
    ]
    (BodyMultipart
       [ Right
           (BodyMultipart
              [ Right
                  (BodyPart
                     (Message
                        [ OptionalField
                            "Content-Type"
                            " text/plain; charset=\"UTF-8\""
                        ]
                        "Here's a smaller file\r\n\r\n"))
              , Right
                  (BodyPart
                     (Message
                        [ OptionalField
                            "Content-Type"
                            " text/html; charset=\"UTF-8\""
                        ]
                        "<div dir=\"ltr\">Here&#39;s a smaller file</div>\r\n\r\n"))
              ])
       , Right
           (BodyPart
              (Message
                 [ OptionalField "Content-Type" " image/png; name=\"image.png\""
                 , OptionalField
                     "Content-Disposition"
                     " attachment; filename=\"image.png\""
                 , OptionalField "Content-Transfer-Encoding" " base64"
                 , OptionalField "Content-ID" " <f_jjsmsed10>"
                 , OptionalField "X-Attachment-Id" " f_jjsmsed10"
                 ]
                 "iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAACXBIWXMAAAsTAAALEwEAmpwYAAAA\r\nB3RJTUUH4gcTDggaWPFqMAAAAUFJREFUGNMBNgHJ/gErITUA/v4BAgIAAwT1/f4CAQEHAf8A/P3+\r\n/f7+//0E6/z//v8BDwUESzcvRkA3BgsKvcTLt7vE/vr6/P0BBAEIDAgJCh0XEUY6MgUICBEiI34/\r\nO/Pw8brCywQBAAMMFCL9+/oNBQPQ2dyBj5kF+vvh4eUICAPW3uX/+fsCAwoMAPv5NhoOMx8XUToy\r\nZVBACwL7B+/rUyca/P//AwMGEgIA/hL9+Qj8/SciGDo3LO/t7TUVC/YDDMXW3QMLFSQB/v0OBwMq\r\nJRzH4OnDztfu3+ABBQeTuMkA/f0ECg0M+O/31Ojt1+rv+f3+9vX2k21arMXP8/P1AgMDAg4PDwED\r\nBPLz8/Dz8/T09C0lHFBWSvL3+fz/APr9/QQEAwUQFBb+8e36+vogFhCGd2cPDwz3AgYMGBf8/v98\r\nL5DQe+nw4AAAAABJRU5ErkJggg==\r\n"))
       ])

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

gmailAttachmentMessage :: ByteString
gmailAttachmentMessage =
  "Received: by mail-qt0-f171.google.com with SMTP id e19-v6so7264767qtp.8\r\n    \
  \    for <gmail@chrisdone.com>; Thu, 19 Jul 2018 07:09:03 -0700 (PDT)\r\nX-Googl\
  \e-DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=1e100.net;\
  \ s=20161025;\r\n        h=x-gm-message-state:delivered-to:dkim-signature:mime-v\
  \ersion\r\n         :reply-to:from:date:message-id:subject:to;\r\n        bh=Oli\
  \/DfRXOO3EH3Cpo48noh3QdZQ2z2xbdYBhVxjaTOI=;\r\n        b=qH82tD41/S0PazkpbjZnRtb\
  \usDRWavXYD/rlhvITJAqniJ9egT1bZPotIwd0vEWV+R\r\n         ns1IJD/nkB0pIYscDPC0SHl\
  \gG5NSf7eT4/N9FXM1nBDOSTEoXluWmEDDNLrA+I/v8LNf\r\n         E/QpR4RObmTh9wWjGc/Yb\
  \d4ZWXY+BA4iUteTr8XYuup3/vBbT+SJuYYPEgFRdCaXP0mn\r\n         sgi3cuChqyZsmbdTZ03\
  \A+Dk0qH0VFdp28e+61t1eDdLMDPiNzbS2hw6wlJIpKmY6Gvh0\r\n         eXYwvShOFq2NmbOkX\
  \Cwxglp7WzlHD+3b93zsgHtRSQWGdz1ESR3c2FtlSg44lYmtG8Ys\r\n         lGMg==\r\nX-Gm-\
  \Message-State: AOUpUlEvxWR7XP0aUlTk7Gjivd1eWOPTNPmdMbdHSK9qBGs8rFEb50Sd\r\n\t4L\
  \zEFkdsMUBwIuOjwdeYH+3yAu/uxstOk9fvBRCPYP1liQL7EAbcCg==\r\nX-Received: by 2002:a\
  \c8:246b:: with SMTP id d40-v6mr9987658qtd.196.1532009343604;\r\n        Thu, 19\
  \ Jul 2018 07:09:03 -0700 (PDT)\r\nX-Forwarded-To: gmail@chrisdone.com\r\nX-Forw\
  \arded-For: chrisdone@gmail.com gmail@chrisdone.com\r\nDelivered-To: chrisdone@g\
  \mail.com\r\nReceived: by 2002:ac8:363:0:0:0:0:0 with SMTP id w35-v6csp1775618qt\
  \g;\r\n        Thu, 19 Jul 2018 07:09:02 -0700 (PDT)\r\nX-Received: by 2002:a0c:\
  \9692:: with SMTP id a18-v6mr10975278qvd.16.1532009342920;\r\n        Thu, 19 Ju\
  \l 2018 07:09:02 -0700 (PDT)\r\nARC-Seal: i=1; a=rsa-sha256; t=1532009342; cv=no\
  \ne;\r\n        d=google.com; s=arc-20160816;\r\n        b=uuOieQD4b/ilUKSb9Nt1y\
  \k6S+p2b3KSaT2y8mHsbT68iEDWYlWggx+7UVofi3UmLWm\r\n         Xb59gox1168w4A4C3tJyd\
  \/5F8/IWWf3ICgkespW76LRz4RFLrOtmSSX8KqXjbqhQgxVO\r\n         Zn7oaBeEyLVAwPXMbCl\
  \5BEJMNnDDyChCrFNDtkIv2zKowqQ304iLUOaTUIpryjzuG4KW\r\n         iyc9ogiISsFGMEsxr\
  \CQiEZx4L2IdVMldeejGGL5j4oCX0HMs9PLSk3wbtVMF+quywobm\r\n         pdYsmlUq5+9DDDn\
  \espiJNyhzm1niNVOPEWT/t3MyHc2WxlzGYWrWy1BNRy1z3V4bkyLa\r\n         ue6A==\r\nARC\
  \-Message-Signature: i=1; a=rsa-sha256; c=relaxed/relaxed; d=google.com; s=arc-2\
  \0160816;\r\n        h=to:subject:message-id:date:from:reply-to:mime-version\r\n\
  \         :dkim-signature:arc-authentication-results;\r\n        bh=Oli/DfRXOO3E\
  \H3Cpo48noh3QdZQ2z2xbdYBhVxjaTOI=;\r\n        b=agS65Xltmi+hxKgmxjiMGtziCdsSW0EM\
  \p3HcYcKzk0TqHJIos2jIwWQ1m1lV2g0FzF\r\n         +Zihia+r5+w35pjzv7/caV3q7NLy92hf\
  \5DJEIUQawduL4Kf4A9eB8ui7sikOxr3XzJD3\r\n         MM0kevphd19djEeuRRBOI91hd909mX\
  \Qv1Xu8yKzPECa0H4vo1CJt/lDltRTtf9fHsS2o\r\n         qlczFBBQBWSb311/PTPwZxDeTR5h\
  \xHp0sCKwrz44hV7h/5onPXPKBrmi82x3KDJyBrAR\r\n         9nLFAt2FbW2zte3kuXszNLU5F7\
  \qJjvBnzGl8Eu4eITAdLK3iHyyV7wn5ldH6fJpYdbNX\r\n         G86A==\r\nARC-Authentica\
  \tion-Results: i=1; mx.google.com;\r\n       dkim=pass header.i=@gmail.com heade\
  \r.s=20161025 header.b=EvyvF+GU;\r\n       spf=pass (google.com: domain of chris\
  \done@gmail.com designates 209.85.220.41 as permitted sender) smtp.mailfrom=chri\
  \sdone@gmail.com;\r\n       dmarc=pass (p=NONE sp=QUARANTINE dis=NONE) header.fr\
  \om=gmail.com\r\nReturn-Path: <chrisdone@gmail.com>\r\nReceived: from mail-sor-f\
  \41.google.com (mail-sor-f41.google.com. [209.85.220.41])\r\n        by mx.googl\
  \e.com with SMTPS id q86-v6sor2954059qkl.125.2018.07.19.07.09.02\r\n        for \
  \<chrisdone@gmail.com>\r\n        (Google Transport Security);\r\n        Thu, 1\
  \9 Jul 2018 07:09:02 -0700 (PDT)\r\nReceived-SPF: pass (google.com: domain of ch\
  \risdone@gmail.com designates 209.85.220.41 as permitted sender) client-ip=209.8\
  \5.220.41;\r\nAuthentication-Results: mx.google.com;\r\n       dkim=pass header.\
  \i=@gmail.com header.s=20161025 header.b=EvyvF+GU;\r\n       spf=pass (google.co\
  \m: domain of chrisdone@gmail.com designates 209.85.220.41 as permitted sender) \
  \smtp.mailfrom=chrisdone@gmail.com;\r\n       dmarc=pass (p=NONE sp=QUARANTINE d\
  \is=NONE) header.from=gmail.com\r\nDKIM-Signature: v=1; a=rsa-sha256; c=relaxed/\
  \relaxed;\r\n        d=gmail.com; s=20161025;\r\n        h=mime-version:reply-to\
  \:from:date:message-id:subject:to;\r\n        bh=Oli/DfRXOO3EH3Cpo48noh3QdZQ2z2x\
  \bdYBhVxjaTOI=;\r\n        b=EvyvF+GUxlJDAgAaVuVSXEmBQBw4SoRakKvtwH8/hlOmJr06Wez\
  \Nw7LnDc18zl9ipy\r\n         Ev6l6QxI5R7C0mNUab0MS5scVPd+6gLstFZLUDDeCoLQvWAJi87\
  \FE/NkjWGGKH8/HGwZ\r\n         8QkS1N/HN+ums0lOZj5qBRLVDnjxUZyIRXy3ll4w6kxwyGqnY\
  \EWaW5xw6ZsM4o5pMoPc\r\n         Rdsr7tf2FJ6PTeZn2c4vUZ7qSluZ4fh6XEq3GKKgX3pCrsU\
  \nGHkOWB6fUeJ2WSP7Mtwu\r\n         4SP9hvoMLJ5RXP45iOKxNXVMPnDf4F45XkfltRnfB6NPq\
  \t1Lt9lRMu8Dq1fLaMn1P9gi\r\n         cdSg==\r\nX-Google-Smtp-Source: AAOMgpeIG6F\
  \5LUaxSpQAVmrSnpyHQbIwtKXJ8Pq9iMYg2hNzDXJD3T6HnaubNg2jziTurWge+pMNfm5Mqu0AgZUo8+\
  \E=\r\nX-Received: by 2002:a37:b506:: with SMTP id e6-v6mr9121547qkf.255.1532009\
  \342201;\r\n Thu, 19 Jul 2018 07:09:02 -0700 (PDT)\r\nMIME-Version: 1.0\r\nReply\
  \-To: chrisdone@googlemail.com\r\nFrom: Christopher Done <chrisdone@gmail.com>\r\
  \\nDate: Thu, 19 Jul 2018 15:08:50 +0100\r\nMessage-ID: <CAAJHNPC_aoWp0AFqHtr2cf\
  \YLsC=uWnQ_iMUNn-hcOdX-SDC-Qg@mail.gmail.com>\r\nSubject: Smaller file\r\nTo: Ch\
  \ris D <chrisdone@gmail.com>\r\nContent-Type: multipart/mixed; boundary=\"000000\
  \000000a2a85205715ab8f1\"\r\n\r\n--000000000000a2a85205715ab8f1\r\nContent-Type:\
  \ multipart/alternative; boundary=\"000000000000a2a84f05715ab8ef\"\r\n\r\n--0000\
  \00000000a2a84f05715ab8ef\r\nContent-Type: text/plain; charset=\"UTF-8\"\r\n\r\n\
  \Here's a smaller file\r\n\r\n--000000000000a2a84f05715ab8ef\r\nContent-Type: te\
  \xt/html; charset=\"UTF-8\"\r\n\r\n<div dir=\"ltr\">Here&#39;s a smaller file</d\
  \iv>\r\n\r\n--000000000000a2a84f05715ab8ef--\r\n--000000000000a2a85205715ab8f1\r\
  \\nContent-Type: image/png; name=\"image.png\"\r\nContent-Disposition: attachmen\
  \t; filename=\"image.png\"\r\nContent-Transfer-Encoding: base64\r\nContent-ID: <\
  \f_jjsmsed10>\r\nX-Attachment-Id: f_jjsmsed10\r\n\r\niVBORw0KGgoAAAANSUhEUgAAAAo\
  \AAAAKCAIAAAACUFjqAAAACXBIWXMAAAsTAAALEwEAmpwYAAAA\r\nB3RJTUUH4gcTDggaWPFqMAAAAU\
  \FJREFUGNMBNgHJ/gErITUA/v4BAgIAAwT1/f4CAQEHAf8A/P3+\r\n/f7+//0E6/z//v8BDwUESzcvR\
  \kA3BgsKvcTLt7vE/vr6/P0BBAEIDAgJCh0XEUY6MgUICBEiI34/\r\nO/Pw8brCywQBAAMMFCL9+/oN\
  \BQPQ2dyBj5kF+vvh4eUICAPW3uX/+fsCAwoMAPv5NhoOMx8XUToy\r\nZVBACwL7B+/rUyca/P//AwM\
  \GEgIA/hL9+Qj8/SciGDo3LO/t7TUVC/YDDMXW3QMLFSQB/v0OBwMq\r\nJRzH4OnDztfu3+ABBQeTuM\
  \kA/f0ECg0M+O/31Ojt1+rv+f3+9vX2k21arMXP8/P1AgMDAg4PDwED\r\nBPLz8/Dz8/T09C0lHFBWS\
  \vL3+fz/APr9/QQEAwUQFBb+8e36+vogFhCGd2cPDwz3AgYMGBf8/v98\r\nL5DQe+nw4AAAAABJRU5E\
  \rkJggg==\r\n--000000000000a2a85205715ab8f1--"
