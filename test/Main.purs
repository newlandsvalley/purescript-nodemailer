module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message) as Exception
import Node.FS.Stream (createReadStream)
import NodeMailer (Message, TransportConfig, createInvalidAccount, createTestAccount, createVerifiedTransporter, getTestMessageUrl, sendMail_)
import NodeMailer.Attachment (Attachment(..))
import NodeMailer.AttachmentStream (fromReadable)

main :: Effect Unit
main = launchAff_ do
  goodConfig <- createTestAccount
  _ <- verifyAndSend goodConfig
  badConfig <- createInvalidAccount
  verifyAndSend badConfig

verifyAndSend :: TransportConfig -> Aff Unit 
verifyAndSend config = do
  eTransporter <- createVerifiedTransporter config
  case eTransporter of 
    Left error -> do
      let 
        errorText = "Connection Error: " <> Exception.message error
      _ <- liftEffect $ log errorText
      pure unit 
    Right transporter -> do 
      message <- liftEffect createMessage
      info <- sendMail_ message transporter
      liftEffect $ log $ "You can confirm a mail at: " <> (show $ getTestMessageUrl info)

createMessage :: Effect Message
createMessage = do
  stream <- fromReadable <$> createReadStream "./test/dummy.png"
  pure
    { from: "noreply@example.com"
    , to: [ "Recipient <recipient@example.com>" ]
    , cc: [ "CCRecipient <ccrecipient@example.com>" ]
    , bcc: [ "BCCRecipient <bccrecipient@example.com>"]
    , subject: "Test Subject"
    , text: "Go to https://github.com"
    , attachments:
        [ FileFromString { filename: "test.txt", content: "TEST" }
        , FileFromPath { filename: "image1.png", path: "./test/dummy.png" }
        , FileFromStream { filename: "image2.png", content: stream }
        ]
    }

