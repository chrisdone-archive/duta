{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Codec.MIME.Base64
import           Codec.MIME.Parse
import qualified Codec.MIME.QuotedPrintable as QP
import           Codec.MIME.Type
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Word
import           Database.Persist.Sqlite hiding (Single)
import           Duta.Model
import           Duta.RFC2047
import qualified Duta.SMTP.Receiver
import qualified Duta.Types.Model
import qualified Spf
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "Libraries"
    (do describe "rfc2047 non-ascii text" rfc2047NonAscii
        describe
          "mime package"
          (do it
                "Codec.MIME.Base64: example roundtrip"
                (shouldBe
                   (let Single s =
                          mime_val_content $
                          parseMIMEMessage $
                          "Content-Type: image/png\r\nContent-Transfer-Encoding: base64\r\n\r\n" <>
                          T.pack (formatOutput 76 Nothing (encodeRaw True image))
                     in S8.pack (T.unpack s))
                   (S.pack image))
              -- The mime package is a bit out of a date and uses String
              -- and Text where a ByteString would be more
              -- appropriate. Due to many conversions, it's not clear
              -- that this will always be correct. Hence, we have a
              -- property test for the roundtrip encode.decode==id.
              it
                "Codec.MIME.Base64: quickcheck roundtrip"
                (property
                   (\bytes ->
                      shouldBe
                        (let Single s =
                               mime_val_content $
                               parseMIMEMessage $
                               "Content-Type: image/png\r\nContent-Transfer-Encoding: base64\r\n\r\n" <>
                               T.pack
                                 (formatOutput 76 Nothing (encodeRaw True bytes))
                          in S8.pack (T.unpack s))
                        (S.pack bytes)))))
  describe
    "Integration"
    (do integrationRegression
        writingToDb)
  describe
    "RFC 2822 Parser"
    (do it
          "GMail example"
          (shouldBe (parseMIMEMessage gmailMessage) gmailMessageParsed)
        it
          "Postfix example"
          (shouldBe (parseMIMEMessage postfixMessage) postfixMessageParsed)
        it
          "GMail Attachment example"
          (shouldBe
             (parseMIMEMessage gmailAttachmentMessage)
             gmailAttachmentParsed)
        it
          "QuotedPrintable parser"
          (shouldBe
             (T.decodeUtf8
                (S8.pack
                   (QP.decode
                      "/home/mlitchard/projects/git/reflex-todo/.stack-work/downloaded/Vheiln5kqwE=\n\
                      \0/src/JSDOM/Custom/XMLHttpRequest.hs:39:46: error:\n\
                      \    =E2=80=A2 Could not deduce (Control.Monad.Catch.MonadThrow\n\
                      \                          Language.Javascript.JSaddle.Types.JSM)\n\
                      \        arising from a use of =E2=80=98throwM=E2=80=99\n\
                      \      from the context: MonadDOM m\n\
                      \        bound by the type signature for:\n\
                      \                   throwXHRError :: MonadDOM m =3D&gt; Maybe XHRError -&gt;=\n\
                      \ m ()\n\
                      \        at src/JSDOM/Custom/XMLHttpRequest.hs:38:1-53\n\
                      \    =E2=80=A2 In the second argument of =E2=80=98(.)=E2=80=99, namely =E2=\n\
                      \=80=98throwM=E2=80=99\n\
                      \      In the second argument of =E2=80=98maybe=E2=80=99, namely =E2=80=98(l=\n\
                      \iftDOM . throwM)=E2=80=99\n\
                      \      In the expression: maybe (return ()) (liftDOM . throwM)")))
             "/home/mlitchard/projects/git/reflex-todo/.stack-work/downloaded/Vheiln5kqwE0/src/JSDOM/Custom/XMLHttpRequest.hs:39:46: error:\n\
                      \    • Could not deduce (Control.Monad.Catch.MonadThrow\n\
                      \                          Language.Javascript.JSaddle.Types.JSM)\n\
                      \        arising from a use of ‘throwM’\n\
                      \      from the context: MonadDOM m\n\
                      \        bound by the type signature for:\n\
                      \                   throwXHRError :: MonadDOM m =&gt; Maybe XHRError -&gt; m ()\n\
                      \        at src/JSDOM/Custom/XMLHttpRequest.hs:38:1-53\n\
                      \    • In the second argument of ‘(.)’, namely ‘throwM’\n\
                      \      In the second argument of ‘maybe’, namely ‘(liftDOM . throwM)’\n\
                      \      In the expression: maybe (return ()) (liftDOM . throwM)")
        it
          "QuotedPrintable parser ending in ="
          (shouldBe
             (T.decodeUtf8
                (S8.pack
                   (QP.decode
                      "xxxxx xxxxxxxxx xx xx xxxxxx xxx xxxxx xx xx xxxxx xxxx xxxx xxxxxxxx =\n\
                      \xxxx. xxxxx xxx xxxx xxxxxxxxx, xxxxxx xxxxxxx xxx xxxxxxxxxxxxxx xxxx =\n\
                      \xxx xx xx. x'xx xx xxxxxxx xx xxxxxxxxxx xxxxxxxxx xxxxx xxxx xx xxxx, =\n\
                      \xxx xx xxx xxx xxxxx xx xxx xxxxxxxxx xxxx xxxx xx xx xxxxxxxxx, xxxxxx =\n\
                      \xxxx xx xx xxxxx.=")))
             "xxxxx xxxxxxxxx xx xx xxxxxx xxx xxxxx xx xx xxxxx xxxx xxxx xxxxxxxx \
              \xxxx. xxxxx xxx xxxx xxxxxxxxx, xxxxxx xxxxxxx xxx xxxxxxxxxxxxxx xxxx \
              \xxx xx xx. x'xx xx xxxxxxx xx xxxxxxxxxx xxxxxxxxx xxxxx xxxx xx xxxx, \
              \xxx xx xxx xxx xxxxx xx xxx xxxxxxxxx xxxx xxxx xx xx xxxxxxxxx, xxxxxx \
              \xxxx xx xx xxxxx."))

rfc2047NonAscii :: Spec
rfc2047NonAscii = do
  it
    "Italian"
    (shouldBe
       (decodeRFC2047 "=?utf-8?q?Scopri_GLC_da_290_=E2=82=AC_al_mese?=")
       "Scopri GLC da 290 \8364 al mese")
  it
    "Chinese"
    (shouldBe
       (decodeRFC2047
          "=?utf-8?B?5ZOl5Lym5q+U5LqaIOepuua0vuWPjOa4heWMheeojiDpnIfm?= =?utf-8?B?krzmnaXooq0=?=")
       "\21733\20262\27604\20122 \31354\27966\21452\28165\21253\31246 \38663\65533\65533\65533\26469\34989")

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
                   , Duta.Types.Model.messageFromHeader =
                       Just "Christopher Done <chrisdone@gmail.com>"
                   , Duta.Types.Model.messageToHeader = Just "wibble@chrisdone.com"
                   , Duta.Types.Model.messageMailFrom = "chrisdone@gmail.com"
                   , Duta.Types.Model.messageHeloDomain = "mail-qk0-f170.google.com"
                   , Duta.Types.Model.messageRcptTo = "wibble@chrisdone.com"
                   , Duta.Types.Model.messageSubject = "Re: wibbling"
                   , Duta.Types.Model.messageParent = Nothing
                   , Duta.Types.Model.messageThread = toSqlKey 1
                   , Duta.Types.Model.messageIp = "127.0.0.1"
                   , Duta.Types.Model.messageIdentifier =
                       Just
                         "<CAAJHNPCnR2LVyN+Ns5TauNTC9Gb1hVnUHGD8+fKstAqm_5yvQA@mail.gmail.com>"
                   }
           (replies, inserted) <-
             runNoLoggingT
               (withSqliteConnInfo
                  (mkSqliteConnectionInfo ":memory:")
                  (runReaderT
                     (do _ <- runMigrationSilent Duta.Types.Model.migrateAll
                         spfServer <- liftIO (Spf.newServer Spf.SPF_DNS_CACHE Spf.ModerateLevel)
                         replies <-
                           C.runConduit
                             (CL.sourceList gmailInput .|
                              Duta.SMTP.Receiver.interaction
                                Duta.SMTP.Receiver.Interaction
                                  { Duta.SMTP.Receiver.interactionHostname = ""
                                  , Duta.SMTP.Receiver.interactionIp =
                                      "127.0.0.1"
                                  , Duta.SMTP.Receiver.interactionOnMessage =
                                      insertModelMessage spfServer
                                  , Duta.SMTP.Receiver.interactionReply =
                                      C.yield
                                  , Duta.SMTP.Receiver.interactionTime = now
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
    (do now <- getCurrentTime
        xs <-
          runNoLoggingT
            (C.runConduit $
             CL.sourceList gmailInput .|
             Duta.SMTP.Receiver.interaction
               Duta.SMTP.Receiver.Interaction
                 { Duta.SMTP.Receiver.interactionHostname = ""
                 , Duta.SMTP.Receiver.interactionIp = "127.0.0.1"
                 , Duta.SMTP.Receiver.interactionOnMessage = \_ -> pure ()
                 , Duta.SMTP.Receiver.interactionReply = C.yield
                 , Duta.SMTP.Receiver.interactionTime = now
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
    (do now <- getCurrentTime
        xs <-
          runNoLoggingT $
          C.runConduit $
          CL.sourceList postfixInput .|
          Duta.SMTP.Receiver.interaction
            Duta.SMTP.Receiver.Interaction
              { Duta.SMTP.Receiver.interactionHostname = ""
              , Duta.SMTP.Receiver.interactionIp = "127.0.0.1"
              , Duta.SMTP.Receiver.interactionOnMessage = \_ -> pure ()
              , Duta.SMTP.Receiver.interactionReply = C.yield
              , Duta.SMTP.Receiver.interactionTime = now
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
  it
    "Regression test against Exim"
    (do now <- getCurrentTime
        xs <-
          runNoLoggingT
            (C.runConduit $
             CL.sourceList eximInput .|
             Duta.SMTP.Receiver.interaction
               Duta.SMTP.Receiver.Interaction
                 { Duta.SMTP.Receiver.interactionHostname = ""
                 , Duta.SMTP.Receiver.interactionIp = "127.0.0.1"
                 , Duta.SMTP.Receiver.interactionOnMessage = \_ -> pure ()
                 , Duta.SMTP.Receiver.interactionReply = C.yield
                 , Duta.SMTP.Receiver.interactionTime = now
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

eximInput :: [ByteString]
eximInput =
  [ "EHLO mail-qk0-f170.google.com\r\n"
  , "MAIL From:<chrisdone@gmail.com>\r\n"
  , "RCPT To:<wibble@chrisdone.com>\r\n"
  , "DATA\r\n"
  , T.encodeUtf8 gmailMessage
  , "QUIT\r\n"
  ]

gmailInput :: [ByteString]
gmailInput =
  [ "EHLO mail-qk0-f170.google.com\r\n"
  , "MAIL FROM:<chrisdone@gmail.com>\r\n"
  , "RCPT TO:<wibble@chrisdone.com>\r\n"
  , "DATA\r\n"
  , T.encodeUtf8 gmailMessage
  , "QUIT\r\n"
  ]

postfixInput :: [ByteString]
postfixInput =
  [ "EHLO somemore.net\r\n"
  , "MAIL FROM:<mihai@bazon.net>\r\n"
  , "RCPT TO:<blah@chrisdone.com>\r\n"
  , "DATA\r\n"
  , T.encodeUtf8 postfixMessage
  , "QUIT\r\n"
  ]

gmailMessageParsed :: MIMEValue
gmailMessageParsed =
  MIMEValue
    { mime_val_type =
        Type
          { mimeType = Multipart Alternative
          , mimeParams =
              [ MIMEParam
                  { paramName = "boundary"
                  , paramValue = "000000000000e1518b0570a23972"
                  }
              ]
          }
    , mime_val_disp = Nothing
    , mime_val_content =
        Multi
          [ MIMEValue
              { mime_val_type =
                  Type
                    { mimeType = Text "plain"
                    , mimeParams =
                        [ MIMEParam
                            {paramName = "charset", paramValue = "UTF-8"}
                        ]
                    }
              , mime_val_disp = Nothing
              , mime_val_content =
                  Single
                    "we continue!\r\n\r\nOn Tue, 10 Jul 2018 at 10:36, Christopher Done <chrisdone@gmail.com> wrote:\r\n\r\n> hey wibble\r\n>\r\n"
              , mime_val_headers =
                  [ MIMEParam
                      { paramName = "content-type"
                      , paramValue = "text/plain; charset=\"UTF-8\""
                      }
                  ]
              , mime_val_inc_type = True
              }
          , MIMEValue
              { mime_val_type =
                  Type
                    { mimeType = Text "html"
                    , mimeParams =
                        [ MIMEParam
                            {paramName = "charset", paramValue = "UTF-8"}
                        ]
                    }
              , mime_val_disp = Nothing
              , mime_val_content =
                  Single
                    "<div dir=\"ltr\">we continue!</div><br><div class=\"gmail_quote\"><div dir=\"ltr\">On Tue, 10 Jul 2018 at 10:36, Christopher Done &lt;<a href=\"mailto:chrisdone@gmail.com\">chrisdone@gmail.com</a>&gt; wrote:<br></div><blockquote class=\"gmail_quote\" style=\"margin:0 0 0 .8ex;border-left:1px #ccc solid;padding-left:1ex\"><div dir=\"ltr\">hey wibble</div>\r\n</blockquote></div>\r\n"
              , mime_val_headers =
                  [ MIMEParam
                      { paramName = "content-type"
                      , paramValue = "text/html; charset=\"UTF-8\""
                      }
                  , MIMEParam
                      { paramName = "content-transfer-encoding"
                      , paramValue = "quoted-printable"
                      }
                  ]
              , mime_val_inc_type = True
              }
          ]
    , mime_val_headers =
        [ MIMEParam
            { paramName = "received"
            , paramValue =
                "by mail-qk0-f170.google.com with SMTP id b66-v6so11206427qkj.1        for <wibble@chrisdone.com>; Tue, 10 Jul 2018 03:02:22 -0700 (PDT)"
            }
        , MIMEParam
            { paramName = "dkim-signature"
            , paramValue =
                "v=1; a=rsa-sha256; c=relaxed/relaxed;        d=gmail.com; s=20161025;        h=mime-version:references:in-reply-to:reply-to:from:date:message-id         :subject:to;        bh=vuaco1M4EZ4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=;        b=NL2PC7xTlI2ampgdRB9B6WzMj/bP+mOvlw/Rtd2+27EMSg5eRdJbs7LjSz8GJV22pl         hF9C8DLTKRo9BrrE6qs27oPMCG0+/eXwOgBQsw/TR2yjvDT2oBnBRkfVjqakVBhmg2GT         Ro+iGjDx8vC1136fI/7A3iXNJnjLHAIYcoMfHEljL7s4hqX8jHzQeWG6+W9jLLH08DFS         IutcEUAnM1DKi8gPP69Qm6i8mEKfHwb8tals8RRthMUhu1w1Hey3djEB5SWpOhU+01BT         fcGxYD10K5ED+T3FfX6CPC+4PMt/7va2ZD8XEfD2Hiogn1unuhjz++jArvq6jWxIxZmB         eQrg=="
            }
        , MIMEParam
            { paramName = "x-google-dkim-signature"
            , paramValue =
                "v=1; a=rsa-sha256; c=relaxed/relaxed;        d=1e100.net; s=20161025;        h=x-gm-message-state:mime-version:references:in-reply-to:reply-to         :from:date:message-id:subject:to;        bh=vuaco1M4EZ4dKC+65I2ne6a/89CLSI3xS3oRZH/qv5s=;        b=soHCjg9EPG07DTHqu/JJyV4+HDqPVNFuLfE56qtggay+5RheihifZ26n/B6CRUSV0x         5C9IxFH1FSaPjkEugcW3wfE39zZyUqro+Ozj1IiB0Za0Srw55Iu5YaYyeEBkSY+VVpdY         NlkNg0/m+gfGo7B05f6CjzqbPQ5iwmOx5kAwS4jKARef0K6Dv2HbeRHYqVCNt66sPGED         zE6TFPxpbdkrDdSdytTevnu00e8IrqvC9rEft6r/nSrP2i0NpdUEuDa+LCFBWfGWwsuV         hrVAtkCTfPOGgJpFCZ+b8uWWqovhnkfWrx0TE0uksW64YqdJXIH9VXGhnWRletjnz6+C         AAyg=="
            }
        , MIMEParam
            { paramName = "x-gm-message-state"
            , paramValue =
                "APt69E11MY8bREVIjYEX2S7HZECtZlfMQ8zqcH1I2F0mDl6fWrrR3grG yhZSXOzxXzHWHKhJEGAtSJyYMDBP063Jz9UTjhaiNQ=="
            }
        , MIMEParam
            { paramName = "x-google-smtp-source"
            , paramValue =
                "AAOMgpeSPyQt6gLoYRDkyjOpo2rN1aBvzv1SdKcmo5DCACWDqPLCUXyocBsaxfYTTsKYRkJp2O9D67xiJMxr5asnlF0="
            }
        , MIMEParam
            { paramName = "x-received"
            , paramValue =
                "by 2002:a37:1028:: with SMTP id a40-v6mr21175009qkh.257.1531216941632; Tue, 10 Jul 2018 03:02:21 -0700 (PDT)"
            }
        , MIMEParam {paramName = "mime-version", paramValue = "1.0"}
        , MIMEParam
            { paramName = "references"
            , paramValue =
                "<CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubWTWSg@mail.gmail.com>"
            }
        , MIMEParam
            { paramName = "in-reply-to"
            , paramValue =
                "<CAAJHNPCBaUTNkaemFyofr=Couam9Eoa-L68jB7p1AUYubWTWSg@mail.gmail.com>"
            }
        , MIMEParam
            {paramName = "reply-to", paramValue = "chrisdone@googlemail.com"}
        , MIMEParam
            { paramName = "from"
            , paramValue = "Christopher Done <chrisdone@gmail.com>"
            }
        , MIMEParam
            {paramName = "date", paramValue = "Tue, 10 Jul 2018 11:02:10 +0100"}
        , MIMEParam
            { paramName = "message-id"
            , paramValue =
                "<CAAJHNPCnR2LVyN+Ns5TauNTC9Gb1hVnUHGD8+fKstAqm_5yvQA@mail.gmail.com>"
            }
        , MIMEParam {paramName = "subject", paramValue = "Re: wibbling"}
        , MIMEParam {paramName = "to", paramValue = "wibble@chrisdone.com"}
        , MIMEParam
            { paramName = "content-type"
            , paramValue =
                "multipart/alternative; boundary=\"000000000000e1518b0570a23972\""
            }
        ]
    , mime_val_inc_type = True
    }

postfixMessageParsed :: MIMEValue
postfixMessageParsed =
  MIMEValue
    { mime_val_type =
        Type
          { mimeType = Text "plain"
          , mimeParams =
              [ MIMEParam {paramName = "charset", paramValue = "utf-8"}
              , MIMEParam {paramName = "format", paramValue = "flowed"}
              ]
          }
    , mime_val_disp = Nothing
    , mime_val_content = Single "blah\r\n\r\n.\r\n"
    , mime_val_headers =
        [ MIMEParam
            { paramName = "received"
            , paramValue =
                "from solution.localdomain (unknown [79.112.116.6]) by somemore.net (Postfix) with ESMTPSA id E796969A3235 for <blah@chrisdone.com>; Mon,  9 Jul 2018 20:48:00 +0300 (EEST)"
            }
        , MIMEParam
            { paramName = "received"
            , paramValue =
                "from [127.0.0.1] (localhost.localdomain [127.0.0.1]) by solution.localdomain (Postfix) with ESMTP id 0D482240326 for <blah@chrisdone.com>; Mon,  9 Jul 2018 20:48:00 +0300 (EEST)"
            }
        , MIMEParam {paramName = "to", paramValue = "blah@chrisdone.com"}
        , MIMEParam
            {paramName = "from", paramValue = "Mihai Bazon <mihai@bazon.net>"}
        , MIMEParam {paramName = "subject", paramValue = "test"}
        , MIMEParam
            { paramName = "message-id"
            , paramValue = "<2c954fd9-7216-4ed5-f303-69b4e811821d@bazon.net>"
            }
        , MIMEParam
            {paramName = "date", paramValue = "Mon, 9 Jul 2018 20:47:59 +0300"}
        , MIMEParam
            { paramName = "user-agent"
            , paramValue =
                "Mozilla/5.0 (X11; Linux x86_64; rv:52.0) Gecko/20100101 Thunderbird/52.8.0"
            }
        , MIMEParam {paramName = "mime-version", paramValue = "1.0"}
        , MIMEParam
            { paramName = "content-type"
            , paramValue = "text/plain; charset=utf-8; format=flowed"
            }
        , MIMEParam
            {paramName = "content-transfer-encoding", paramValue = "7bit"}
        , MIMEParam {paramName = "content-language", paramValue = "en-US"}
        ]
    , mime_val_inc_type = True
    }

gmailAttachmentParsed :: MIMEValue
gmailAttachmentParsed =
  MIMEValue
    { mime_val_type =
        Type
          { mimeType = Multipart Mixed
          , mimeParams =
              [ MIMEParam
                  { paramName = "boundary"
                  , paramValue = "000000000000a2a85205715ab8f1"
                  }
              ]
          }
    , mime_val_disp = Nothing
    , mime_val_content =
        Multi
          [ MIMEValue
              { mime_val_type =
                  Type
                    { mimeType = Multipart Alternative
                    , mimeParams =
                        [ MIMEParam
                            { paramName = "boundary"
                            , paramValue = "000000000000a2a84f05715ab8ef"
                            }
                        ]
                    }
              , mime_val_disp = Nothing
              , mime_val_content =
                  Multi
                    [ MIMEValue
                        { mime_val_type =
                            Type
                              { mimeType = Text "plain"
                              , mimeParams =
                                  [ MIMEParam
                                      { paramName = "charset"
                                      , paramValue = "UTF-8"
                                      }
                                  ]
                              }
                        , mime_val_disp = Nothing
                        , mime_val_content = Single "Here's a smaller file\r\n"
                        , mime_val_headers =
                            [ MIMEParam
                                { paramName = "content-type"
                                , paramValue = "text/plain; charset=\"UTF-8\""
                                }
                            ]
                        , mime_val_inc_type = True
                        }
                    , MIMEValue
                        { mime_val_type =
                            Type
                              { mimeType = Text "html"
                              , mimeParams =
                                  [ MIMEParam
                                      { paramName = "charset"
                                      , paramValue = "UTF-8"
                                      }
                                  ]
                              }
                        , mime_val_disp = Nothing
                        , mime_val_content =
                            Single
                              "<div dir=\"ltr\">Here&#39;s a smaller file</div>\r\n"
                        , mime_val_headers =
                            [ MIMEParam
                                { paramName = "content-type"
                                , paramValue = "text/html; charset=\"UTF-8\""
                                }
                            ]
                        , mime_val_inc_type = True
                        }
                    ]
              , mime_val_headers =
                  [ MIMEParam
                      { paramName = "content-type"
                      , paramValue =
                          "multipart/alternative; boundary=\"000000000000a2a84f05715ab8ef\""
                      }
                  ]
              , mime_val_inc_type = True
              }
          , MIMEValue
              { mime_val_type =
                  Type
                    { mimeType = Image "png"
                    , mimeParams =
                        [ MIMEParam
                            {paramName = "name", paramValue = "image.png"}
                        ]
                    }
              , mime_val_disp =
                  Just
                    (Disposition
                       { dispType = DispAttachment
                       , dispParams = [Filename "image.png"]
                       })
              , mime_val_content =
                  Single
                    "\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL\n\NUL\NUL\NUL\n\b\STX\NUL\NUL\NUL\STXPX\234\NUL\NUL\NUL\tpHYs\NUL\NUL\v\DC3\NUL\NUL\v\DC3\SOH\NUL\154\156\CAN\NUL\NUL\NUL\atIME\a\226\a\DC3\SO\b\SUBX\241j0\NUL\NUL\SOHAIDAT\CAN\211\SOH6\SOH\201\254\SOH+!5\NUL\254\254\SOH\STX\STX\NUL\ETX\EOT\245\253\254\STX\SOH\SOH\a\SOH\255\NUL\252\253\254\253\254\254\255\253\EOT\235\252\255\254\255\SOH\SI\ENQ\EOTK7/F@7\ACK\v\n\189\196\203\183\187\196\254\250\250\252\253\SOH\EOT\SOH\b\f\b\t\n\GS\ETB\DC1F:2\ENQ\b\b\DC1\"#~?;\243\240\241\186\194\203\EOT\SOH\NUL\ETX\f\DC4\"\253\251\250\r\ENQ\ETX\208\217\220\129\143\153\ENQ\250\251\225\225\229\b\b\ETX\214\222\229\255\249\251\STX\ETX\n\f\NUL\251\249\&6\SUB\SO3\US\ETBQ:2eP@\v\STX\251\a\239\235S'\SUB\252\255\255\ETX\ETX\ACK\DC2\STX\NUL\254\DC2\253\249\b\252\253'\"\CAN:7,\239\237\237\&5\NAK\v\246\ETX\f\197\214\221\ETX\v\NAK$\SOH\254\253\SO\a\ETX*%\FS\199\224\233\195\206\215\238\223\224\SOH\ENQ\a\147\184\201\NUL\253\253\EOT\n\r\f\248\239\247\212\232\237\215\234\239\249\253\254\246\245\246\147mZ\172\197\207\243\243\245\STX\ETX\ETX\STX\SO\SI\SI\SOH\ETX\EOT\242\243\243\240\243\243\244\244\244-%\FSPVJ\242\247\249\252\255\NUL\250\253\253\EOT\EOT\ETX\ENQ\DLE\DC4\SYN\254\241\237\250\250\250 \SYN\DLE\134wg\SI\SI\f\247\STX\ACK\f\CAN\ETB\252\254\255|/\144\208{\233\240\224\NUL\NUL\NUL\NULIEND\174B`\130"
              , mime_val_headers =
                  [ MIMEParam
                      { paramName = "content-type"
                      , paramValue = "image/png; name=\"image.png\""
                      }
                  , MIMEParam
                      { paramName = "content-disposition"
                      , paramValue = "attachment; filename=\"image.png\""
                      }
                  , MIMEParam
                      { paramName = "content-transfer-encoding"
                      , paramValue = "base64"
                      }
                  , MIMEParam
                      {paramName = "content-id", paramValue = "<f_jjsmsed10>"}
                  , MIMEParam
                      { paramName = "x-attachment-id"
                      , paramValue = "f_jjsmsed10"
                      }
                  ]
              , mime_val_inc_type = True
              }
          ]
    , mime_val_headers =
        [ MIMEParam
            { paramName = "received"
            , paramValue =
                "by mail-qt0-f171.google.com with SMTP id e19-v6so7264767qtp.8        for <gmail@chrisdone.com>; Thu, 19 Jul 2018 07:09:03 -0700 (PDT)"
            }
        , MIMEParam
            { paramName = "x-google-dkim-signature"
            , paramValue =
                "v=1; a=rsa-sha256; c=relaxed/relaxed;        d=1e100.net; s=20161025;        h=x-gm-message-state:delivered-to:dkim-signature:mime-version         :reply-to:from:date:message-id:subject:to;        bh=Oli/DfRXOO3EH3Cpo48noh3QdZQ2z2xbdYBhVxjaTOI=;        b=qH82tD41/S0PazkpbjZnRtbusDRWavXYD/rlhvITJAqniJ9egT1bZPotIwd0vEWV+R         ns1IJD/nkB0pIYscDPC0SHlgG5NSf7eT4/N9FXM1nBDOSTEoXluWmEDDNLrA+I/v8LNf         E/QpR4RObmTh9wWjGc/Ybd4ZWXY+BA4iUteTr8XYuup3/vBbT+SJuYYPEgFRdCaXP0mn         sgi3cuChqyZsmbdTZ03A+Dk0qH0VFdp28e+61t1eDdLMDPiNzbS2hw6wlJIpKmY6Gvh0         eXYwvShOFq2NmbOkXCwxglp7WzlHD+3b93zsgHtRSQWGdz1ESR3c2FtlSg44lYmtG8Ys         lGMg=="
            }
        , MIMEParam
            { paramName = "x-gm-message-state"
            , paramValue =
                "AOUpUlEvxWR7XP0aUlTk7Gjivd1eWOPTNPmdMbdHSK9qBGs8rFEb50Sd 4LzEFkdsMUBwIuOjwdeYH+3yAu/uxstOk9fvBRCPYP1liQL7EAbcCg=="
            }
        , MIMEParam
            { paramName = "x-received"
            , paramValue =
                "by 2002:ac8:246b:: with SMTP id d40-v6mr9987658qtd.196.1532009343604;        Thu, 19 Jul 2018 07:09:03 -0700 (PDT)"
            }
        , MIMEParam
            {paramName = "x-forwarded-to", paramValue = "gmail@chrisdone.com"}
        , MIMEParam
            { paramName = "x-forwarded-for"
            , paramValue = "chrisdone@gmail.com gmail@chrisdone.com"
            }
        , MIMEParam
            {paramName = "delivered-to", paramValue = "chrisdone@gmail.com"}
        , MIMEParam
            { paramName = "received"
            , paramValue =
                "by 2002:ac8:363:0:0:0:0:0 with SMTP id w35-v6csp1775618qtg;        Thu, 19 Jul 2018 07:09:02 -0700 (PDT)"
            }
        , MIMEParam
            { paramName = "x-received"
            , paramValue =
                "by 2002:a0c:9692:: with SMTP id a18-v6mr10975278qvd.16.1532009342920;        Thu, 19 Jul 2018 07:09:02 -0700 (PDT)"
            }
        , MIMEParam
            { paramName = "arc-seal"
            , paramValue =
                "i=1; a=rsa-sha256; t=1532009342; cv=none;        d=google.com; s=arc-20160816;        b=uuOieQD4b/ilUKSb9Nt1yk6S+p2b3KSaT2y8mHsbT68iEDWYlWggx+7UVofi3UmLWm         Xb59gox1168w4A4C3tJyd/5F8/IWWf3ICgkespW76LRz4RFLrOtmSSX8KqXjbqhQgxVO         Zn7oaBeEyLVAwPXMbCl5BEJMNnDDyChCrFNDtkIv2zKowqQ304iLUOaTUIpryjzuG4KW         iyc9ogiISsFGMEsxrCQiEZx4L2IdVMldeejGGL5j4oCX0HMs9PLSk3wbtVMF+quywobm         pdYsmlUq5+9DDDnespiJNyhzm1niNVOPEWT/t3MyHc2WxlzGYWrWy1BNRy1z3V4bkyLa         ue6A=="
            }
        , MIMEParam
            { paramName = "arc-message-signature"
            , paramValue =
                "i=1; a=rsa-sha256; c=relaxed/relaxed; d=google.com; s=arc-20160816;        h=to:subject:message-id:date:from:reply-to:mime-version         :dkim-signature:arc-authentication-results;        bh=Oli/DfRXOO3EH3Cpo48noh3QdZQ2z2xbdYBhVxjaTOI=;        b=agS65Xltmi+hxKgmxjiMGtziCdsSW0EMp3HcYcKzk0TqHJIos2jIwWQ1m1lV2g0FzF         +Zihia+r5+w35pjzv7/caV3q7NLy92hf5DJEIUQawduL4Kf4A9eB8ui7sikOxr3XzJD3         MM0kevphd19djEeuRRBOI91hd909mXQv1Xu8yKzPECa0H4vo1CJt/lDltRTtf9fHsS2o         qlczFBBQBWSb311/PTPwZxDeTR5hxHp0sCKwrz44hV7h/5onPXPKBrmi82x3KDJyBrAR         9nLFAt2FbW2zte3kuXszNLU5F7qJjvBnzGl8Eu4eITAdLK3iHyyV7wn5ldH6fJpYdbNX         G86A=="
            }
        , MIMEParam
            { paramName = "arc-authentication-results"
            , paramValue =
                "i=1; mx.google.com;       dkim=pass header.i=@gmail.com header.s=20161025 header.b=EvyvF+GU;       spf=pass (google.com: domain of chrisdone@gmail.com designates 209.85.220.41 as permitted sender) smtp.mailfrom=chrisdone@gmail.com;       dmarc=pass (p=NONE sp=QUARANTINE dis=NONE) header.from=gmail.com"
            }
        , MIMEParam
            {paramName = "return-path", paramValue = "<chrisdone@gmail.com>"}
        , MIMEParam
            { paramName = "received"
            , paramValue =
                "from mail-sor-f41.google.com (mail-sor-f41.google.com. [209.85.220.41])        by mx.google.com with SMTPS id q86-v6sor2954059qkl.125.2018.07.19.07.09.02        for <chrisdone@gmail.com>        (Google Transport Security);        Thu, 19 Jul 2018 07:09:02 -0700 (PDT)"
            }
        , MIMEParam
            { paramName = "received-spf"
            , paramValue =
                "pass (google.com: domain of chrisdone@gmail.com designates 209.85.220.41 as permitted sender) client-ip=209.85.220.41;"
            }
        , MIMEParam
            { paramName = "authentication-results"
            , paramValue =
                "mx.google.com;       dkim=pass header.i=@gmail.com header.s=20161025 header.b=EvyvF+GU;       spf=pass (google.com: domain of chrisdone@gmail.com designates 209.85.220.41 as permitted sender) smtp.mailfrom=chrisdone@gmail.com;       dmarc=pass (p=NONE sp=QUARANTINE dis=NONE) header.from=gmail.com"
            }
        , MIMEParam
            { paramName = "dkim-signature"
            , paramValue =
                "v=1; a=rsa-sha256; c=relaxed/relaxed;        d=gmail.com; s=20161025;        h=mime-version:reply-to:from:date:message-id:subject:to;        bh=Oli/DfRXOO3EH3Cpo48noh3QdZQ2z2xbdYBhVxjaTOI=;        b=EvyvF+GUxlJDAgAaVuVSXEmBQBw4SoRakKvtwH8/hlOmJr06WezNw7LnDc18zl9ipy         Ev6l6QxI5R7C0mNUab0MS5scVPd+6gLstFZLUDDeCoLQvWAJi87FE/NkjWGGKH8/HGwZ         8QkS1N/HN+ums0lOZj5qBRLVDnjxUZyIRXy3ll4w6kxwyGqnYEWaW5xw6ZsM4o5pMoPc         Rdsr7tf2FJ6PTeZn2c4vUZ7qSluZ4fh6XEq3GKKgX3pCrsUnGHkOWB6fUeJ2WSP7Mtwu         4SP9hvoMLJ5RXP45iOKxNXVMPnDf4F45XkfltRnfB6NPqt1Lt9lRMu8Dq1fLaMn1P9gi         cdSg=="
            }
        , MIMEParam
            { paramName = "x-google-smtp-source"
            , paramValue =
                "AAOMgpeIG6F5LUaxSpQAVmrSnpyHQbIwtKXJ8Pq9iMYg2hNzDXJD3T6HnaubNg2jziTurWge+pMNfm5Mqu0AgZUo8+E="
            }
        , MIMEParam
            { paramName = "x-received"
            , paramValue =
                "by 2002:a37:b506:: with SMTP id e6-v6mr9121547qkf.255.1532009342201; Thu, 19 Jul 2018 07:09:02 -0700 (PDT)"
            }
        , MIMEParam {paramName = "mime-version", paramValue = "1.0"}
        , MIMEParam
            {paramName = "reply-to", paramValue = "chrisdone@googlemail.com"}
        , MIMEParam
            { paramName = "from"
            , paramValue = "Christopher Done <chrisdone@gmail.com>"
            }
        , MIMEParam
            {paramName = "date", paramValue = "Thu, 19 Jul 2018 15:08:50 +0100"}
        , MIMEParam
            { paramName = "message-id"
            , paramValue =
                "<CAAJHNPC_aoWp0AFqHtr2cfYLsC=uWnQ_iMUNn-hcOdX-SDC-Qg@mail.gmail.com>"
            }
        , MIMEParam {paramName = "subject", paramValue = "Smaller file"}
        , MIMEParam
            {paramName = "to", paramValue = "Chris D <chrisdone@gmail.com>"}
        , MIMEParam
            { paramName = "content-type"
            , paramValue =
                "multipart/mixed; boundary=\"000000000000a2a85205715ab8f1\""
            }
        ]
    , mime_val_inc_type = True
    }

gmailMessage :: Text
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

postfixMessage :: Text
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

gmailAttachmentMessage :: Text
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
  \AAAAKCAIAAAACUFjqAAAACXBIWXMAAAsTAAALEwEAmpwYAAAA\r\nB3RJTUUH4gcTDggaWPFqMAAAAUFJREFUGNMBNgHJ/gErITUA/v4BAgIAAwT1/f4CAQEHAf8A/P3+\r\n/f7+//0E6/z//v8BDwUESzcvR\
  \kA3BgsKvcTLt7vE/vr6/P0BBAEIDAgJCh0XEUY6MgUICBEiI34/\r\nO/Pw8brCywQBAAMMFCL9+/oN\
  \BQPQ2dyBj5kF+vvh4eUICAPW3uX/+fsCAwoMAPv5NhoOMx8XUToy\r\nZVBACwL7B+/rUyca/P//AwM\
  \GEgIA/hL9+Qj8/SciGDo3LO/t7TUVC/YDDMXW3QMLFSQB/v0OBwMq\r\nJRzH4OnDztfu3+ABBQeTuM\
  \kA/f0ECg0M+O/31Ojt1+rv+f3+9vX2k21arMXP8/P1AgMDAg4PDwED\r\nBPLz8/Dz8/T09C0lHFBWS\
  \vL3+fz/APr9/QQEAwUQFBb+8e36+vogFhCGd2cPDwz3AgYMGBf8/v98\r\nL5DQe+nw4AAAAABJRU5E\
  \rkJggg==\r\n--000000000000a2a85205715ab8f1--"

image :: [Word8]
image =
  S.unpack
    "\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL\n\NUL\NUL\NUL\n\b\STX\NUL\NUL\NUL\STXPX\234\NUL\NUL\NUL\tpHYs\NUL\NUL\v\DC3\NUL\NUL\v\DC3\SOH\NUL\154\156\CAN\NUL\NUL\NUL\atIME\a\226\a\DC3\SO\b\SUBX\241j0\NUL\NUL\SOHAIDAT\CAN\211\SOH6\SOH\201\254\SOH+!5\NUL\254\254\SOH\STX\STX\NUL\ETX\EOT\245\253\254\STX\SOH\SOH\a\SOH\255\NUL\252\253\254\253\254\254\255\253\EOT\235\252\255\254\255\SOH\SI\ENQ\EOTK7/F@7\ACK\v\n\189\196\203\183\187\196\254\250\250\252\253\SOH\EOT\SOH\b\f\b\t\n\GS\ETB\DC1F:2\ENQ\b\b\DC1\"#~?;\243\240\241\186\194\203\EOT\SOH\NUL\ETX\f\DC4\"\253\251\250\r\ENQ\ETX\208\217\220\129\143\153\ENQ\250\251\225\225\229\b\b\ETX\214\222\229\255\249\251\STX\ETX\n\f\NUL\251\249\&6\SUB\SO3\US\ETBQ:2eP@\v\STX\251\a\239\235S'\SUB\252\255\255\ETX\ETX\ACK\DC2\STX\NUL\254\DC2\253\249\b\252\253'\"\CAN:7,\239\237\237\&5\NAK\v\246\ETX\f\197\214\221\ETX\v\NAK$\SOH\254\253\SO\a\ETX*%\FS\199\224\233\195\206\215\238\223\224\SOH\ENQ\a\147\184\201\NUL\253\253\EOT\n\r\f\248\239\247\212\232\237\215\234\239\249\253\254\246\245\246\147mZ\172\197\207\243\243\245\STX\ETX\ETX\STX\SO\SI\SI\SOH\ETX\EOT\242\243\243\240\243\243\244\244\244-%\FSPVJ\242\247\249\252\255\NUL\250\253\253\EOT\EOT\ETX\ENQ\DLE\DC4\SYN\254\241\237\250\250\250 \SYN\DLE\134wg\SI\SI\f\247\STX\ACK\f\CAN\ETB\252\254\255|/\144\208{\233\240\224\NUL\NUL\NUL\NULIEND\174B`\130"
