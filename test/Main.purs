module Test.Main where

import Prelude

import Data.Either (Either(..), isRight)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message) as Exception
import Node.FS.Stream (createReadStream)
import NodeMailer (Message, TransportConfig, createTestAccount, createTransporter, createVerifiedTransporter, getTestMessageUrl, sendMailMessage)
import NodeMailer.Attachment (Attachment(..))
import NodeMailer.AttachmentStream (fromReadable)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ specReporter ] do
  describe "nodemailer" do
    mailSpec

mailSpec :: Spec Unit
mailSpec =
  describe "mail spec" do
    it "emails using a verified connection with correct user details" do
      goodConfig <- createTestAccount 
      eResult <- verifyAndSend goodConfig
      eResult `shouldSatisfy` isRight
    it "emails using an unverified connection with correct user details" do
      goodConfig <- createTestAccount 
      eResult <- sendUnverified goodConfig
      eResult `shouldSatisfy` isRight
    it "verifies a bad connection before attempting to send" do
      badConfig <- createInvalidAccount
      eResult <- verifyAndSend badConfig
      eResult `shouldEqual` (Left "Connection Error: Invalid login: 535 Authentication failed")
    it "reports a send failure on an unverified connection" do
      badConfig <- createInvalidAccount
      eResult <- sendUnverified badConfig
      eResult `shouldEqual` (Left "Send Error: Invalid login: 535 Authentication failed")

verifyAndSend :: TransportConfig -> Aff (Either String Unit)
verifyAndSend config = do
  eTransporter <- createVerifiedTransporter config
  case eTransporter of 
    Left error -> do
      let 
        errorText = "Connection Error: " <> Exception.message error
      _ <- liftEffect $ log errorText
      pure $ Left errorText
    Right transporter -> do 
      message <- liftEffect createMessage
      eInfo <- sendMailMessage message transporter
      case eInfo of 
        Left error -> do
          let 
            errorText = "Send Error: " <> Exception.message error
          _ <- liftEffect $ log errorText
          pure $ Left errorText
        Right info -> do
          _ <- liftEffect $ log $ "You can confirm a mail at: " <> (show $ getTestMessageUrl info)
          pure $ Right unit

sendUnverified :: TransportConfig -> Aff (Either String Unit)
sendUnverified config = do
  transporter <- liftEffect $ createTransporter config
  message <- liftEffect createMessage
  eResult <- sendMailMessage message transporter

  case eResult of 
    Left error -> do
      let 
        errorText = "Send Error: " <> Exception.message error
      _ <- liftEffect $ log errorText
      pure $ Left errorText
    Right info -> do 
      _ <- liftEffect $ log $ "You can confirm a mail at: " <> (show $ getTestMessageUrl info)
      pure $ Right unit

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

-- | create an account for the ethereum test harness with a bad password
createInvalidAccount:: Aff TransportConfig
createInvalidAccount = do
  account <- createTestAccount
  let 
    badAuth = account.auth { pass = "nopassword"}
    badAccount = account { auth = badAuth}
  pure badAccount



