{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api (startServer) where

-- Base / standard libraries

import qualified Control.Exception as E


-- Web libraries
import Web.Scotty
import Network.Wai.Parse (lbsBackEnd, parseRequestBody)
import Network.HTTP.Types (internalServerError500, status400, unauthorized401)

-- JSON handling
import Data.Aeson (object, (.=))

-- Project modules
import Database (connectDB, createTables)
import Handlers (getUserHandler, getUserLoginHandler, editUserHandler, createFriendRequestHandler, getFriendsByStatusHandler, acceptFriendRequestHandler, refuseFriendRequestHandler, userSignupHandler, deleteImageHandler, uploadImageHandler, getImageHandler)
import Types (Login (..), NewUser (..), EditUser (..))

startServer :: IO ()
startServer = do
  conn <- connectDB
  createTables conn
  -- createTestUser conn
  scotty 3000 $ do
    -- Home route
    get "/" $ do
      text "Oiiii ta funcionando :)"

    -- Get a single user by ID / mostly for testing
    get "/users" $ do
      uidParam <- param "id"
      let uid = (read uidParam :: Int)
      maybeUser <- liftIO $ getUserHandler conn uid
      case maybeUser of
        Left errorMsg -> json (object ["message" .= errorMsg])
        Right user -> json user
    
    -- Login route
    post "/login" $ do
      bodyData <- jsonData :: ActionM Login
      let Login {username = username', password = password'} = bodyData
      maybeUser <- liftIO $ getUserLoginHandler conn username' password'
      case maybeUser of
        Left errorMsg -> do
          status unauthorized401
          json (object ["error" .= errorMsg])
        Right user -> json user

    -- Edit user by ID
    post "/editUser" $ do
      bodyData <- jsonData :: ActionM EditUser
      -- liftIO $ print bodyData
      let EditUser {userId = uid} = bodyData
      maybeUser <- liftIO $ editUserHandler conn uid bodyData
      case maybeUser of
        Left errorMsg -> do
          status internalServerError500
          json (object ["message" .= errorMsg])
        Right editUser -> json editUser

    put "/signup" $ do
      bodyData <- jsonData :: ActionM NewUser
      -- Printing received data for debugging
      liftIO $ print bodyData
      -- Constructing NewUser from parsed JSON
      let NewUser {username = username', email = email', password = password', birthday = birthday'} = bodyData
      -- Calling handler to sign up user
      result <- liftIO $ userSignupHandler conn bodyData
      case result of
        Left errorMsg -> do
          status unauthorized401
          json (object ["message" .= errorMsg])
        Right successMsg -> json (object ["message" .= successMsg])

    -- Image get route, serving images directly from the "images" directory
    get "/image" $ do
      maybeImageId <- param "imageId" `rescue` (\(_ :: E.SomeException) -> return "")
      result <- liftIO $ getImageHandler maybeImageId
      case result of
        Left errorMsg -> do
          status unauthorized401
          json (object ["message" .= errorMsg])
        Right filePath -> file filePath

    -- Processing image upload route
    post "/image" $ do
      req <- request
      (_, files) <- liftIO $ parseRequestBody lbsBackEnd req
      result <- liftIO $ uploadImageHandler files
      case result of
        Left errorMsg -> do
          status status400
          json (object ["message" .= errorMsg])
        Right imageId -> json (object ["imageId" .= imageId])

    -- Deletes image
    delete "/image" $ do
      maybeLast <- param "lastImageId" `rescue` (\(_ :: E.SomeException) -> return "")
      result <- liftIO $ deleteImageHandler maybeLast
      case result of
        Left errorMsg -> json (object ["message" .= errorMsg])
        Right successMsg -> json (object ["message" .= successMsg])

    -- Get friend request by id and status
    get "/friends" $ do
      userId <- param "userId"
      statusParam <- param "status"
      friendsRequests <- liftIO $ getFriendsByStatusHandler conn userId statusParam
      json friendsRequests

    -- Accept friend request
    post "/friend/request/accept" $ do
      userId <- param "userId"
      friendId <- param "friendId"
      success <- liftIO $ acceptFriendRequestHandler conn userId friendId
      case success of
        Left errMsg -> do
          status internalServerError500
          json (object ["message" .= errMsg])
        Right successMsg -> json (object ["message" .= successMsg])

    -- Reject friend request
    post "/friend/request/refuse" $ do
      userId <- param "userId"
      friendId <- param "friendId"
      success <- liftIO $ refuseFriendRequestHandler conn userId friendId
      case success of
        Left errMsg -> do
          status internalServerError500
          json (object ["message" .= errMsg])
        Right successMsg -> json (object ["message" .= successMsg])

    -- Send friend request
    post "/friend/request" $ do
      userId <- param "userId"
      friendUsername <- param "friendUsername"
      success <- liftIO $ createFriendRequestHandler conn userId friendUsername
      case success of
        Left errMsg -> do
          status internalServerError500
          json (object ["message" .= errMsg])
        Right successMsg -> json (object ["message" .= successMsg])
