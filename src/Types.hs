{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (User (..), Login (..), NewUser (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (Day)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)

-- User data type
data User = User
  { userId :: Int,
    username :: Text,
    nickname :: Maybe Text,
    email :: Text,
    password :: Text,
    birthday :: Day,
    biography :: Maybe Text,
    profileImage :: Maybe Text,
    backgroundImage :: Maybe Text,
    createdDate :: Day,
    deleted :: Bool
  }
  deriving (Show, Generic)

-- Converting MySQL row to User
instance FromRow User where
  fromRow =
    User
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance ToJSON User

data NewUser = NewUser
  { username :: Text,
    nickname :: Maybe Text,
    email :: Text,
    password :: Text,
    birthday :: Day,
    biography :: Maybe Text,
    profileImage :: Maybe Text,
    backgroundImage :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON NewUser

instance ToRow NewUser where
  toRow (NewUser u n e p b bio pi bi) =
    [ toField u,
      toField n,
      toField e,
      toField p,
      toField b,
      toField bio,
      toField pi,
      toField bi
    ]
-- Login request data type
data Login = Login
  { email :: Text,
    password :: Text
  }
  deriving (Show, Generic)

instance FromJSON Login
