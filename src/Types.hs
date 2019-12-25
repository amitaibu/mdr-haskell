{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.Aeson (object, (.=), ToJSON, toJSON)
import RIO
import RIO.Process
import Data.UUID (UUID, toText)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

data Mother = Mother
    { motherFirstName :: !Text
    , motherLastName :: !Text
    , motherChildren :: ![ Child ]
    , motherId :: !Text
    } deriving (Show, Eq)

data Child = Child
    { childFirstName :: !Text
    , childLastName :: !Text
    , childId :: !Text
    } deriving (Show, Eq)


data ChildWithMotherId = ChildWithMotherId Child UUID


instance ToJSON Mother where
    toJSON obj =
        object [ "id" .= motherId obj
               , "first_name" .= motherFirstName obj
               , "last_name" .= motherLastName obj
               , "children" .= children
               ]
        where
            children = fmap (\child -> childId child) (motherChildren obj)

-- @todo: Why can't we do ToJSON (UUID, Child)?
instance ToJSON ChildWithMotherId where
    toJSON (ChildWithMotherId obj uuid) =
        object [ "id" .= childId obj
               , "first_name" .= childFirstName obj
               , "last_name" .= childLastName obj
               , "mother" .= toText uuid
               ]