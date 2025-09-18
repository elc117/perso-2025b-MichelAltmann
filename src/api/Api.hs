{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api (startServer) where

-- Base / standard libraries
import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import System.Random (randomIO)
import qualified Control.Exception as E
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Text and bytestring handling
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- Web libraries
import Web.Scotty
import Network.Wai
import Network.Wai.Parse (fileContent, fileName, lbsBackEnd, parseRequestBody)
import Network.HTTP.Types (internalServerError500, status400, status404, unauthorized401)

-- JSON handling
import Data.Aeson (object, (.=))

-- Project modules
import Database (connectDB, createTables, createTestUser)
import Handlers (getUserHandler, getUserLoginHandler, getEmailHandler, createUserHandler, getUsernameHandler, editUserHandler, createFriendRequestHandler, getFriendsRequestHandler, HandlerResult(..))
import Types (Login (..), NewUser (..), User (..), EditUser (..))
import Utils (userToJson)

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
    get "/users" $ do
      uidParam <- param "id"
      let uid = (read uidParam :: Int)
      maybeUser <- liftIO $ getUserHandler conn uid
      case maybeUser of
        Just user -> json (userToJson user)
        Nothing -> text "User not found"
    
    -- Edit user by ID
    post "/editUser" $ do
      bodyData <- jsonData :: ActionM EditUser
      -- Printing received data for debugging
      liftIO $ print bodyData
      let EditUser {userId = uid} = bodyData
      maybeUser <- liftIO $ editUserHandler conn uid bodyData
      case maybeUser of
        Just editUser -> do
          liftIO $ print editUser
          json editUser
        Nothing -> do
          status internalServerError500
          json (object ["message" .= ("Failed to edit user" :: String)])

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

    -- Image get route, serving images directly from the "images" directory
    get "/image" $ do
      maybeImageId <- param "imageId" `rescue` (\(_ :: E.SomeException) -> return "")
      if TL.null maybeImageId
        then status status400 >> json (object ["message" .= ("Bad request." :: T.Text)])
        else do
          let imagePath = "images" </> TL.unpack maybeImageId
          fileExists <- liftIO $ doesFileExist imagePath
          if fileExists
            then file imagePath
            else status status404 >> json (object ["message" .= ("Image not found." :: T.Text)])

    -- Processing image upload route
    post "/image" $ do
      req <- request
      (arams, files) <- liftIO $ parseRequestBody lbsBackEnd req
      case files of
        [] -> status status400 >> json (object ["message" .= ("Bad request" :: String)])
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
          json $ object ["imageId" .= newName]

    -- Deletes image
    delete "/image" $ do
      maybeLast <- param "lastImageId" `rescue` (\(_ :: E.SomeException) -> return "")
      if TL.null maybeLast
        then status status400 >> json (object ["message" .= ("Bad request." :: String)])
        else do
          let imagePath = "images" </> TL.unpack maybeLast
          exists <- liftIO $ doesFileExist imagePath
          if not exists
            then status status404 >> json (object ["message" .= ("Image not found." :: String)])
            else do
              liftIO $ removeFile imagePath
              json $ object ["message" .= ("Image deleted successfully." :: String)]

    -- Send friend request
    post "/friend/request" $ do
      userId <- param "userId"
      friendUsername <- param "friendUsername"
      maybeError <- liftIO $ createFriendRequestHandler conn userId friendUsername
      case maybeError of
        Nothing -> json (object ["message" .= ("Friend request sent successfully" :: String)])
        Just errMsg -> do
          status unauthorized401
          json (object ["message" .= errMsg])