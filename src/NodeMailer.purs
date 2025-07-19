module NodeMailer
  ( AuthConfig
  , TransportConfig
  , Message
  , Transporter
  , MessageInfo
  , createTransporter
  , createVerifiedTransporter
  , createTestAccount
  , createInvalidAccount
  , getTestMessageUrl
  , sendMail
  , sendMail_
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

type AuthConfig =
  { user :: String
  , pass :: String
  }

type TransportConfig =
  { host :: String
  , port :: Int
  , secure :: Boolean
  , auth :: AuthConfig
  }

type TestAccount =
  { user :: String
  , pass :: String
  , smtp :: { host :: String, port :: Int, secure :: Boolean }
  }

type Message =
  { from :: String
  , to :: Array String
  , cc :: Array String
  , bcc :: Array String
  , subject :: String
  , text :: String
  , attachments :: Array Attachment
  }

foreign import data Transporter :: Type

foreign import data MessageInfo :: Type

-- | create an unverified transporter from the config
createTransporter :: TransportConfig -> Effect Transporter
createTransporter config = runEffectFn1 createTransporterImpl config

sendMail :: Message -> Transporter -> Aff Unit
sendMail message transporter = void $ sendMail_ message transporter

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

createTestAccount :: Aff TransportConfig
createTestAccount = do
  account <- fromEffectFnAff createTestAccountImpl
  pure
    { host: account.smtp.host
    , port: account.smtp.port
    , secure: account.smtp.secure
    , auth: { user: account.user, pass: account.pass }
    }

createInvalidAccount :: Aff TransportConfig
createInvalidAccount = do
  account <- fromEffectFnAff createTestAccountImpl
  pure
    { host: account.smtp.host
    , port: account.smtp.port
    , secure: account.smtp.secure
    , auth: { user: account.user, pass: "nopassword" }
    }

getTestMessageUrl :: MessageInfo -> Maybe String
getTestMessageUrl = runFn3 getTestMessageUrlImpl Nothing Just

foreign import createTransporterImpl :: EffectFn1 TransportConfig Transporter

foreign import sendMailImpl :: Fn2 Foreign Transporter (EffectFnAff MessageInfo)

foreign import verifyImpl :: Transporter -> EffectFnAff Boolean

foreign import createTestAccountImpl :: EffectFnAff TestAccount

foreign import getTestMessageUrlImpl
  :: Fn3 (Maybe String) (String -> Maybe String) MessageInfo (Maybe String)
