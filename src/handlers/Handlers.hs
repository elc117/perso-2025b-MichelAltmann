{-# LANGUAGE OverloadedStrings #-}

module Handlers
  ( getUserHandler,
    getUserLoginHandler,
    getEmailHandler,
    createUserHandler,
    getUsernameHandler,
    editUserHandler,
    createFriendRequestHandler,
    getFriendsRequestHandler,
    getFriendsByStatusHandler,
    HandlerResult (..),
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Database (createFriendRequest, createUser, editUser, getEmail, getFriendRequest, getFriendsByStatus, getUserById, getUserLogin, getUsername, getUserIdByUsername)
import Database.SQLite.Simple (Connection)
import Types (EditUser (..), Friend (..), NewUser (..), User (..), FriendRequest (..))
import Web.Scotty

-- Handler for getting user from database using ID
getUserHandler :: Connection -> Int -> IO (Maybe User)
getUserHandler conn uid = getUserById conn uid

-- Handler for getting user from database using email and password
getUserLoginHandler :: Connection -> T.Text -> T.Text -> IO (Maybe User)
getUserLoginHandler conn email password = getUserLogin conn email password

-- Handler for editing user by ID
editUserHandler :: Connection -> Int -> EditUser -> IO (Maybe EditUser)
editUserHandler conn uid user = editUser conn uid user

-- Handler for checking if email exists in the database
getEmailHandler :: Connection -> T.Text -> IO (Maybe T.Text)
getEmailHandler conn email = getEmail conn email

-- Handler for checking if username exists in the database
getUsernameHandler :: Connection -> T.Text -> IO (Maybe T.Text)
getUsernameHandler conn username = getUsername conn username

-- Handler for creating new user
createUserHandler :: Connection -> NewUser -> IO Bool
createUserHandler conn newUser = createUser conn newUser

-- Handler for creating a friend request
createFriendRequestHandler :: Connection -> Int -> T.Text -> IO (Maybe T.Text)
createFriendRequestHandler conn userId friendUsername = do
  maybeFriendUserId <- getUserIdByUsername conn friendUsername
  case maybeFriendUserId of
    Nothing -> return $ Just "Friend username does not exist"
    Just friendUserId -> do
      if userId == friendUserId
        then return $ Just "Cannot add yourself as friend"
        else do
          success <- createFriendRequest conn userId friendUserId
          if success
            then return Nothing -- Success, no error
            else return $ Just "Friend request already exists"

-- Handler for getting friend requests
getFriendsRequestHandler :: Connection -> Int -> Int -> IO (Bool)
getFriendsRequestHandler conn userId friendUserId = getFriendRequest conn userId friendUserId

-- Handler for getting friends by status
getFriendsByStatusHandler :: Connection -> Int -> Int -> IO [Friend]
getFriendsByStatusHandler conn userId status = getFriendsByStatus conn userId status