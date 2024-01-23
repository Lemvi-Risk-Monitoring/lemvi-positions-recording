{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module Echo (app) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Servant.API
    ( JSON,
      Required,
      Strict,
      QueryParam',
      type (:>),
      Get,
      type (:<|>)(..) )

import Servant.Server ( Server, Application, serve, Handler )
import Data.Proxy (Proxy (..))


type Greet =
  "greet"
    :> QueryParam' '[Required, Strict] "person" Text
    :> Get '[JSON] Message

type Hoot = "hoot" :> Get '[JSON] Message

greet :: Server Greet
greet person = pure . Message $ "Hello, " <> person <> "!"

hoot :: Server Hoot
hoot = pure $ Message "Hoot!"

newtype Message = Message Text

instance ToJSON Message where
  toJSON (Message t) = object ["message" .= t]

type Api = Echo.Greet :<|> Echo.Hoot

handlers :: (Text -> Handler Echo.Message) :<|> Handler Echo.Message
handlers = Echo.greet :<|> Echo.hoot

app :: Application
app = serve (Proxy :: Proxy Api) handlers
