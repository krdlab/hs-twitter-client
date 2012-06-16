{-# LANGUAGE OverloadedStrings #-}
module Hstter.Model where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Text (Text)

data Status = Status
  { text      :: Text
  , createdAt :: ByteString
  , user      :: User
  , idStr     :: ByteString
  }
  deriving (Show, Eq)

data User = User
  { screenName  :: ByteString
  , name        :: ByteString
  }
  deriving (Show, Eq)

instance FromJSON Status where
  parseJSON (Object v) = Status
                          <$> v .: "text"
                          <*> v .: "created_at"
                          <*> v .: "user"
                          <*> v .: "id_str"
  parseJSON _          = mzero

instance FromJSON User where
  parseJSON (Object v) = User
                          <$> v .: "screen_name"
                          <*> v .: "name"
  parseJSON _          = mzero

