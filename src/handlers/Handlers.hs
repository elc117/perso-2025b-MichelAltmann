{-# LANGUAGE OverloadedStrings #-}

module Handlers (getUserHandler, getUserLoginHandler, getEmailHandler, createUserHandler, getUsernameHandler) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Aeson (object, (.=))
import Database (getUserById, getUserLogin, getEmail, createUser, getUsername)
import Database.SQLite.Simple (Connection)
import Types (User (..), NewUser (..))
import Web.Scotty

-- Handler for getting user from database using ID
getUserHandler :: Connection -> Int -> IO (Maybe User)
getUserHandler conn uid = getUserById conn uid

-- Handler for getting user from database using email and password
getUserLoginHandler :: Connection -> T.Text -> T.Text -> IO (Maybe User)
getUserLoginHandler conn email password = getUserLogin conn email password

-- Handler for checking if email exists in the database
getEmailHandler :: Connection -> T.Text -> IO (Maybe T.Text)
getEmailHandler conn email = getEmail conn email

-- Handler for checking if username exists in the database
getUsernameHandler :: Connection -> T.Text -> IO (Maybe T.Text)
getUsernameHandler conn username = getUsername conn username

-- Handler for creating new user
createUserHandler :: Connection -> NewUser -> IO Bool
createUserHandler conn newUser = createUser conn newUser