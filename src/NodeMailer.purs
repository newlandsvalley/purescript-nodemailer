module NodeMailer
  ( AuthConfig
  , TransportConfig
  , Message
  , Transporter
  , MessageInfo
  , createTransporter
  , createVerifiedTransporter
  , createTestAccount
  , getTestMessageUrl
  , sendMail
  , sendMail_
  , sendMailMessage
  , verifyConnection
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, catchError)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign (Foreign)
import NodeMailer.Attachment (Attachment)
import Simple.JSON (write)

-- | user authorization configuration
type AuthConfig =
  { user :: String
  , pass :: String
  }

-- | transport (i.e. connection) configuration
type TransportConfig =
  { host :: String
  , port :: Int
  , secure :: Boolean
  , auth :: AuthConfig
  }

-- | just the private internal details defining connection to the Ethereal tool
type TestAccount =
  { user :: String
  , pass :: String
  , smtp :: { host :: String, port :: Int, secure :: Boolean }
  }

-- | a mail message
type Message =
  { from :: String
  , to :: Array String
  , cc :: Array String
  , bcc :: Array String
  , subject :: String
  , text :: String
  , attachments :: Array Attachment
  }

-- | The mail Transporter - i.e. the senders connection details
foreign import data Transporter :: Type

-- | information about the sent message
foreign import data MessageInfo :: Type

-- | create an unverified transporter from the config
createTransporter :: TransportConfig -> Effect Transporter
createTransporter config = runEffectFn1 createTransporterImpl config

-- | send a mail message and return either an Error or MsgInfo
sendMailMessage :: Message -> Transporter -> Aff (Either Error MessageInfo)
sendMailMessage message transporter = do
  discriminatedSendMail `catchError` \e -> handleError e

  where

  discriminatedSendMail:: Aff (Either Error MessageInfo)
  discriminatedSendMail = do
    msgInfo <- sendMail_ message transporter
    pure $ Right msgInfo

  handleError :: Error -> Aff (Either Error MessageInfo)
  handleError e = do
    pure $ Left e

-- | send a mail message, ignoring errors and returning void
-- | @deprecated in favour of sendMailMessage - this will eventually be removed but kept for backward compatibilty
sendMail :: Message -> Transporter -> Aff Unit
sendMail message transporter = void $ sendMail_ message transporter

-- | send a mail message, ignoring errors and returning MsgInfo
-- | @deprecated in favour of sendMailMessage - this will eventually be removed but kept for backward compatibilty
sendMail_ :: Message -> Transporter -> Aff MessageInfo
sendMail_ message transporter = fromEffectFnAff $ runFn2 sendMailImpl (write message) transporter

-- | Verify the transporter connection
verifyConnection ::  Transporter -> Aff (Either Error Transporter)
verifyConnection transporter = do 
  discriminatedVerify  `catchError` \e -> handleError e

  where

  discriminatedVerify:: Aff (Either Error Transporter)
  discriminatedVerify = do
    _result <- verify
    pure $ Right transporter

  handleError :: Error -> Aff (Either Error Transporter)
  handleError e = do
    pure $ Left e

  verify :: Aff Boolean
  verify = fromEffectFnAff $ verifyImpl transporter

-- | create a verified transporter
createVerifiedTransporter :: TransportConfig -> Aff (Either Error Transporter)
createVerifiedTransporter config = do
  unverifiedTransporter <- liftEffect $ createTransporter config 
  verifyConnection unverifiedTransporter

-- | create a test account to the Ethereal development tool
createTestAccount :: Aff TransportConfig
createTestAccount = do
  account <- fromEffectFnAff createTestAccountImpl
  pure
    { host: account.smtp.host
    , port: account.smtp.port
    , secure: account.smtp.secure
    , auth: { user: account.user, pass: account.pass }
    }


-- | get the Ethereal link to a successfully sent message in testing environments
getTestMessageUrl :: MessageInfo -> Maybe String
getTestMessageUrl = runFn3 getTestMessageUrlImpl Nothing Just

foreign import createTransporterImpl :: EffectFn1 TransportConfig Transporter

foreign import sendMailImpl :: Fn2 Foreign Transporter (EffectFnAff MessageInfo)

foreign import verifyImpl :: Transporter -> EffectFnAff Boolean

foreign import createTestAccountImpl :: EffectFnAff TestAccount

foreign import getTestMessageUrlImpl
  :: Fn3 (Maybe String) (String -> Maybe String) MessageInfo (Maybe String)
