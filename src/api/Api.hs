{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api (startServer) where

import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (unauthorized401, internalServerError500)
import Data.Aeson (decode, object, (.=))
import Database (connectDB, createTables, createTestUser)
import Handlers (getUserHandler, getUserLoginHandler, getEmailHandler, createUserHandler, getUsernameHandler)
import Types (Login (..), User (..), NewUser (..))
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
      -- Constructing NewUser from parsed JSON
      let NewUser{ username = username', nickname = nickname', email = email', password = password', birthday = birthday', biography = biography', profileImage = profileImage', backgroundImage = backgroundImage' } = bodyData

      maybeEmail <- liftIO $ getEmailHandler conn email'
      maybeUsername <- liftIO $ getUsernameHandler conn username'
      -- Checking if email or username already exists in the db
      case (maybeEmail, maybeUsername) of
        -- Checking email
        (Just _, _) -> do
          status unauthorized401
          json (object ["error" .= ("Email already exists" :: String)])
        -- Checking username
        (_, Just _) -> do
          status unauthorized401
          json (object ["error" .= ("Username already exists" :: String)])
        -- If both are available create the user
        (Nothing, Nothing) -> do
          -- Insert new user into the database
          success <- liftIO $ createUserHandler conn bodyData
          if success
            then json (object ["success" .= ("User created successfully" :: String)])
            else do
              status internalServerError500
              json (object ["error" .= ("Failed to create user" :: String)])

    -- Login route
    post "/login" $ do
      bodyData <- jsonData :: ActionM Login
      let Login { email = email', password = password' } = bodyData

      maybeUser <- liftIO $ getUserLoginHandler conn email' password'
      case maybeUser of
        Just user -> json (userToJson user)
        Nothing -> do
          status unauthorized401
          json (object ["error" .= ("Invalid email or password" :: String)])