{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}


module PostSQS (initEnvAWS, defaultEnvAWS, sendMessageRegion, sendMessage, createQueue, requestQueueURL) where

import qualified Amazonka as AWS
import qualified Amazonka.SQS as SQS
import Control.Lens ( (<&>), view, set )
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.Resource ( ResourceT )
import Data.Generics.Labels ()
import qualified Data.Text as T
import qualified Data.Text.IO as Text
import System.IO ( stdout )

say :: T.Text -> ResourceT IO ()
say = liftIO . Text.putStrLn

initEnvAWS :: AWS.Region -> IO AWS.Env
initEnvAWS reg = do
  lgr <- AWS.newLogger AWS.Debug stdout
  AWS.newEnv AWS.discover <&> set #logger lgr . set #region reg

defaultEnvAWS :: IO AWS.Env
defaultEnvAWS = do
  lgr <- AWS.newLogger AWS.Debug stdout
  AWS.newEnv AWS.discover <&> set #logger lgr

sendMessageRegion :: AWS.Region -> T.Text -> T.Text -> IO ()
sendMessageRegion reg queueName message = do
  env <- initEnvAWS reg
  AWS.runResourceT $ sendMessageEnv env queueName message

sendMessage :: T.Text -> T.Text -> IO ()
sendMessage queueName message = do
  env <- defaultEnvAWS
  AWS.runResourceT $ sendMessageEnv env queueName message

sendMessageEnv :: AWS.Env -> T.Text -> T.Text -> ResourceT IO ()
sendMessageEnv env queueName message = do
    url <- view #queueUrl <$> requestQueueURL env queueName
    say $ "Using Queue URL: " <> url
    void $ AWS.send env (SQS.newSendMessage url message)
    say $ "Sent '" <> message <> "' to Queue URL: " <> url

createQueue :: AWS.Env -> T.Text -> ResourceT IO SQS.CreateQueueResponse
createQueue env queueName = AWS.send env $ SQS.newCreateQueue queueName

requestQueueURL :: AWS.Env -> T.Text -> ResourceT IO SQS.GetQueueUrlResponse
requestQueueURL env queueName = AWS.send env $ SQS.newGetQueueUrl queueName
