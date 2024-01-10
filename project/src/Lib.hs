{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

import GHC.Generics
import Data.Aeson
import Aws.Lambda

data Person = Person
  { personName :: String
  , personAge :: Int
  } deriving (Generic)

instance FromJSON Person
instance ToJSON Person

handler :: Person -> Context () -> IO (Either String Person)
handler person context =
  if personAge person > 0 then
    return (Right person)
  else
    return (Left "A person's age must be positive")
