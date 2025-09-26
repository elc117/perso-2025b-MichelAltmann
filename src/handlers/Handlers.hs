{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers
  ( getUserHandler,
    getUserLoginHandler,
    userSignupHandler,
    getUsernameHandler,
    editUserHandler,
    createFriendRequestHandler,
    getFriendsByStatusHandler,
    acceptFriendRequestHandler,
    refuseFriendRequestHandler,
    deleteImageHandler,
    uploadImageHandler,
    getImageHandler
  )
where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import System.Random (randomIO)



import Network.Wai.Parse (File,fileContent, fileName, )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX (getPOSIXTime)


import Control.Monad.IO.Class (liftIO)
import Database (createFriendRequest, createUser, editUser, getEmail, getFriendsByStatus, getUserById, getUserLogin, getUsername, getUserIdByUsername, acceptFriendRequest, refuseFriendRequest)
import Database.SQLite.Simple (Connection)
import Types (EditUser (..), Friend (..), NewUser (..), User (..))

-- Handler for getting user from database using ID
getUserHandler :: Connection -> Int -> IO (Either T.Text User)
getUserHandler conn uid = do 
  maybeUser <- getUserById conn uid
  case maybeUser of
    Nothing -> return (Left "User not found")
    Just user -> return (Right user)

-- Handler for getting user from database using email and password
getUserLoginHandler :: Connection -> T.Text -> T.Text -> IO (Either T.Text User)
getUserLoginHandler conn email password = do 
  maybeLogin <- getUserLogin conn email password
  case maybeLogin of
    Nothing -> return (Left "Invalid email or password")
    Just user -> return (Right user)

-- Handler for editing user by ID
editUserHandler :: Connection -> Int -> EditUser -> IO (Either T.Text EditUser)
editUserHandler conn uid user =  do 
  maybeEditUser <- editUser conn uid user
  case maybeEditUser of
    Nothing -> return (Left "Failed to edit user")
    Just editUser -> return (Right editUser)

-- Handler for checking if email exists in the database
getEmailHandler :: Connection -> T.Text -> IO (Either T.Text T.Text)
getEmailHandler conn email = do
  maybeEmail <- getEmail conn email
  case maybeEmail of
    Nothing -> return (Left "Email not found")
    Just email -> return (Right email)

-- Handler for checking if username exists in the database
getUsernameHandler :: Connection -> T.Text -> IO (Either T.Text T.Text)
getUsernameHandler conn username = do
  maybeUsername <- getUsername conn username
  case maybeUsername of
    Nothing -> return (Left "Username not found")
    Just username -> return (Right username)

-- Handler for creating new user
createUserHandler :: Connection -> NewUser -> IO Bool
createUserHandler conn newUser = createUser conn newUser

-- Handler for signing up user
userSignupHandler :: Connection -> NewUser -> IO (Either T.Text T.Text)
userSignupHandler conn newUser = do
  let NewUser { email = newUserEmail, username = newUserUsername } = newUser
  emailCheck <- getEmailHandler conn newUserEmail
  usernameCheck <- getUsernameHandler conn newUserUsername
  case (emailCheck, usernameCheck) of
    (Left _, Left _) -> do
      success <- createUserHandler conn newUser
      case success of
        False -> return $ Left "Failed to create user"
        True -> return $ Right "User created successfully"
    (Right _, _) -> return (Left "Email already in use")
    (_, Right _) -> return (Left "Username already in use")

-- Handler for getting images
getImageHandler :: TL.Text -> IO (Either T.Text FilePath)
getImageHandler maybeImageId = do
  if TL.null maybeImageId
        then return (Left "Bad request.")
        else do
          let imagePath = "images" </> TL.unpack maybeImageId
          fileExists <- liftIO $ doesFileExist imagePath
          if fileExists
            then return (Right imagePath)
            else return (Left "Image not found.")

-- Handler for uploading images
uploadImageHandler :: [File BL.ByteString] -> IO (Either T.Text T.Text)
uploadImageHandler files = do
  case files of
        [] -> return $ Left "Bad request"
        (_, fi) : _ -> do
          -- Generating unique filename
          ts <- liftIO $ round <$> getPOSIXTime
          rnd <- liftIO (randomIO :: IO Int)
          let origName = BS.unpack (fileName fi)
              newName = origName ++ "_" ++ show ts ++ "_" ++ show rnd ++ ".jpg"
              destPath = "images" </> newName
          -- Creating directory if it doesn't exist yet
          liftIO $ createDirectoryIfMissing True "images"
          liftIO $ BL.writeFile destPath (fileContent fi)
          return $ Right (T.pack newName)

-- Handler for deleting images
deleteImageHandler :: TL.Text -> IO (Either T.Text T.Text)
deleteImageHandler path = do
  if TL.null path
    then return (Left "Bad request.")
    else do
      let imagePath = "images" </> TL.unpack path
      exists <- liftIO $ doesFileExist imagePath
      if not exists
        then return (Left "Image not found.")
        else do
          liftIO $ removeFile imagePath
          return $ Right "Image deleted successfully."

-- Handler for creating a friend request
createFriendRequestHandler :: Connection -> Int -> T.Text -> IO (Either T.Text T.Text)
createFriendRequestHandler conn userId friendUsername = do
  maybeFriendUserId <- getUserIdByUsername conn friendUsername
  case maybeFriendUserId of
    Nothing -> return $ Left "Friend username does not exist"
    Just friendUserId -> do
      if userId == friendUserId
        then return $ Left "Cannot add yourself as friend"
        else do
          success <- createFriendRequest conn userId friendUserId
          if success
            then return $ Right "Friend request sent successfully"
            else return $ Left "Friend request already exists"

-- Handler for getting friends by status
getFriendsByStatusHandler :: Connection -> Int -> Int -> IO [Friend]
getFriendsByStatusHandler conn userId status = getFriendsByStatus conn userId status

-- Handler for accepting a friend request
acceptFriendRequestHandler :: Connection -> Int -> Int -> IO (Either T.Text T.Text)
acceptFriendRequestHandler conn userId friendUserId = do 
  accepted <- acceptFriendRequest conn userId friendUserId
  case accepted of
    False -> return (Left "Failed to accept friend request")
    True -> return (Right "Friend request accepted")

-- Handler for refusing a friend request
refuseFriendRequestHandler :: Connection -> Int -> Int -> IO (Either T.Text T.Text)
refuseFriendRequestHandler conn userId friendUserId = do
  refused <- refuseFriendRequest conn userId friendUserId
  case refused of
    False -> return (Left "Failed to refuse friend request")
    True -> return (Right "Friend request refused")