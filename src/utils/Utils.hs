{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Data.Time (Day)
import Types (User (..))

-- Function to parse User to JSON
userToJson :: User -> Value
userToJson user =
  object
    [ "id" .= userId user,
      "username" .= username user,
      "nickname" .= nickname user,
      "email" .= email user,
      "password" .= password user,
      "birthday" .= show (birthday user),
      "biography" .= biography user,
      "profileImage" .= profileImage user,
      "backgroundImage" .= backgroundImage user,
      "createdDate" .= show (createdDate user),
      "deleted" .= deleted user
    ]