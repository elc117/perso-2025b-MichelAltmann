{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Api (startServer) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, object, (.=))
import qualified Data.Text as T
import Database (connectDB, createTables, createTestUser)
import Handlers (createUserHandler, getEmailHandler, getUserHandler, getUserLoginHandler, getUsernameHandler)
import Network.HTTP.Types (internalServerError500, unauthorized401)
import Types (Login (..), NewUser (..), User (..))
import Utils (userToJson)
import Web.Scotty

startServer :: IO ()
startServer = do
  conn <- connectDB
  createTables conn
  createTestUser conn
  scotty 3000 $ do
    -- Home route
    get "/" $ do
      text "Oiiii ta funcionando :)"

    -- Get a single user by ID
    get "/users/:id" $ do
      uidParam <- param "id"
      let uid = (read uidParam :: Int)
      maybeUser <- liftIO $ getUserHandler conn uid
      case maybeUser of
        Just user -> json (userToJson user)
        Nothing -> text "User not found"

    put "/signup" $ do
      bodyData <- jsonData :: ActionM NewUser
      -- Printing received data for debugging
      liftIO $ print bodyData

      -- Constructing NewUser from parsed JSON
      let NewUser {username = username', email = email', password = password', birthday = birthday'} = bodyData

      maybeEmail <- liftIO $ getEmailHandler conn email'
      maybeUsername <- liftIO $ getUsernameHandler conn username'
      -- Checking if email or username already exists in the db
      case (maybeEmail, maybeUsername) of
        -- Checking email
        (Just _, _) -> do
          status unauthorized401
          json (object ["message" .= ("Email already in use" :: String)])
        -- Checking username
        (_, Just _) -> do
          status unauthorized401
          json (object ["message" .= ("Username already in use" :: String)])
        -- If both are available create the user
        (Nothing, Nothing) -> do
          -- Insert new user into the database
          success <- liftIO $ createUserHandler conn bodyData
          if success
            then json (object ["message" .= ("User created successfully" :: String)])
            else do
              status internalServerError500
              json (object ["message" .= ("Failed to create user" :: String)])

    -- Login route
    post "/login" $ do
      bodyData <- jsonData :: ActionM Login
      let Login {username = username', password = password'} = bodyData

      -- Printing received data for debugging
      liftIO $ print bodyData

      maybeUser <- liftIO $ getUserLoginHandler conn username' password'
      case maybeUser of
        Just user -> json (userToJson user)
        Nothing -> do
          status unauthorized401
          json (object ["error" .= ("Invalid username or password" :: String)])