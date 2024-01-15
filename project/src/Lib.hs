{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON )
import Aws.Lambda ( Context )

data Person = Person
  { personName :: String
  , personAge :: Int
  } deriving (Generic)

instance FromJSON Person
instance ToJSON Person

handler :: Person -> Context () -> IO (Either String Person)
handler person _ =
  if personAge person > 0 then
    return (Right person)
  else
    return (Left "A person's age must be positive")
