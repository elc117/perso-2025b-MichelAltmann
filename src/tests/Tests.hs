{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Database.SQLite.Simple
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Exception (bracket)
import Data.Time.Calendar (fromGregorian)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)

import Database (connectDB, createTables, createUser)
import Handlers (
    getUserHandler,
    getUserLoginHandler,
    editUserHandler,
    userSignupHandler,
    createFriendRequestHandler,
    acceptFriendRequestHandler,
    refuseFriendRequestHandler,
    getFriendsByStatusHandler,
    deleteImageHandler,
    uploadImageHandler,
    getImageHandler
  )
import Types (NewUser(..), User, EditUser(..))

-- Helper function to create a test database
testDB :: IO Connection
testDB = do
  conn <- open ":memory:"
  createTables conn
  return conn

-- Helper to create a test user and return user ID
createTestUser :: Connection -> T.Text -> T.Text -> T.Text -> IO ()
createTestUser conn username email password = do
  let testUser = NewUser username email password (fromGregorian 1990 1 1)
  _ <- createUser conn testUser
  return ()

-- Test getUserHandler with existing user
test_getUserHandler_success :: Test
test_getUserHandler_success = TestCase $ do
  conn <- testDB
  -- Create test user using proper handler
  createTestUser conn "testuser" "test@example.com" "password123"
  
  result <- getUserHandler conn 1
  case result of
    Left err -> assertFailure $ "Expected success, got error: " ++ T.unpack err
    Right user -> assertBool "Should return a user" True
  close conn

-- Test getUserHandler with non-existent user  
test_getUserHandler_notfound :: Test
test_getUserHandler_notfound = TestCase $ do
  conn <- testDB
  result <- getUserHandler conn 999 -- non Existing ID
  case result of
    Left err -> assertEqual "Should return user not found" "User not found" err
    Right _ -> assertFailure "Expected failure for non-existent user"
  close conn

-- Test getUserLoginHandler with valid credentials
test_getUserLoginHandler_success :: Test
test_getUserLoginHandler_success = TestCase $ do
  conn <- testDB
  createTestUser conn "loginuser" "login@example.com" "mypassword"
  
  result <- getUserLoginHandler conn "loginuser" "mypassword"  -- Use username, not email
  case result of
    Left err -> assertFailure $ "Expected success, got error: " ++ T.unpack err
    Right user -> assertBool "Should return a user" True
  close conn

-- Test getUserLoginHandler with invalid credentials
test_getUserLoginHandler_invalid :: Test
test_getUserLoginHandler_invalid = TestCase $ do
  conn <- testDB
  createTestUser conn "loginuser" "login@example.com" "mypassword"
  
  result <- getUserLoginHandler conn "loginuser" "wrongpassword"  -- Use username, not email
  case result of
    Left err -> assertEqual "Should return invalid credentials" "Invalid email or password" err
    Right _ -> assertFailure "Expected failure for wrong password"
  close conn

-- Test editUserHandler with valid data
test_editUserHandler_success :: Test
test_editUserHandler_success = TestCase $ do
  conn <- testDB
  createTestUser conn "newuser" "new@example.com" "newpass123"

  let updatedUser = EditUser 1 "newuser" (Just "newnickname") "new@example.com" (fromGregorian 2004 9 5) (Just "New bio") Nothing Nothing

  result <- editUserHandler conn 1 updatedUser
  case result of
    Left err -> assertFailure $ "Expected success, got error: " ++ T.unpack err
    Right _ -> assertBool "Should return updated user" True
  close conn

-- Test editUserHandler with non-existent user
test_editUserHandler_failure :: Test
test_editUserHandler_failure = TestCase $ do
  conn <- testDB
  let updatedUser = EditUser 999 "nonexistent" Nothing "nonexistent@example.com" (fromGregorian 2004 9 5) Nothing Nothing Nothing

  result <- editUserHandler conn 999 updatedUser
  case result of
    Left err -> assertEqual "Should return user not found" "Failed to edit user" err
    Right _ -> assertFailure "Expected failure for non-existent user"
  close conn

-- Test userSignupHandler with new user
test_userSignupHandler_success :: Test
test_userSignupHandler_success = TestCase $ do
  conn <- testDB
  let newUser = NewUser "newuser" "new@example.com" "newpass123" (fromGregorian 2004 9 5)
  
  result <- userSignupHandler conn newUser
  case result of
    Left err -> assertFailure $ "Expected success, got error: " ++ T.unpack err
    Right msg -> assertEqual "Should return success message" "User created successfully" msg
  close conn

-- Test userSignupHandler with duplicate email
test_userSignupHandler_duplicate_email :: Test
test_userSignupHandler_duplicate_email = TestCase $ do
  conn <- testDB
  -- Create first user
  createTestUser conn "user1" "duplicate@example.com" "pass1"
  
  -- Try to create user with same email
  let duplicateUser = NewUser "user2" "duplicate@example.com" "pass2" (fromGregorian 2004 9 5)
  result <- userSignupHandler conn duplicateUser
  case result of
    Left err -> assertEqual "Should return email in use error" "Email already in use" err
    Right _ -> assertFailure "Expected failure for duplicate email"
  close conn

-- Test userSignupHandler with duplicate username
test_userSignupHandler_duplicate_username :: Test
test_userSignupHandler_duplicate_username = TestCase $ do
  conn <- testDB
  -- Create first user
  createTestUser conn "duplicate" "user1@example.com" "pass1"
  
  -- Try to create user with same username
  let duplicateUser = NewUser "duplicate" "user2@example.com" "pass2" (fromGregorian 2004 9 5)
  result <- userSignupHandler conn duplicateUser
  case result of
    Left err -> assertEqual "Should return username in use error" "Username already in use" err
    Right _ -> assertFailure "Expected failure for duplicate username"
  close conn

-- Test createFriendRequestHandler success
test_createFriendRequestHandler_success :: Test
test_createFriendRequestHandler_success = TestCase $ do
  conn <- testDB
  -- Create two users
  createTestUser conn "user1" "user1@example.com" "pass1"
  createTestUser conn "user2" "user2@example.com" "pass2"
  
  result <- createFriendRequestHandler conn 1 "user2"
  case result of
    Left err -> assertFailure $ "Expected success, got error: " ++ T.unpack err
    Right msg -> assertEqual "Should return success message" "Friend request sent successfully" msg
  close conn

-- Test createFriendRequestHandler with non-existent friend
test_createFriendRequestHandler_notfound :: Test
test_createFriendRequestHandler_notfound = TestCase $ do
  conn <- testDB
  createTestUser conn "user1" "user1@example.com" "pass1"
  
  result <- createFriendRequestHandler conn 1 "nonexistent"
  case result of
    Left err -> assertEqual "Should return friend not found" "Friend username does not exist" err
    Right _ -> assertFailure "Expected failure for non-existent friend"
  close conn

-- Test createFriendRequestHandler self-friend attempt
test_createFriendRequestHandler_self :: Test
test_createFriendRequestHandler_self = TestCase $ do
  conn <- testDB
  createTestUser conn "user1" "user1@example.com" "pass1"
  
  result <- createFriendRequestHandler conn 1 "user1"
  case result of
    Left err -> assertEqual "Should return cannot add self" "Cannot add yourself as friend" err
    Right _ -> assertFailure "Expected failure for self-friend request"
  close conn

-- Test getFriendsByStatusHandler with no friends
test_getFriendsByStatusHandler_no_friends :: Test
test_getFriendsByStatusHandler_no_friends = TestCase $ do
  conn <- testDB
  createTestUser conn "user1" "user1@example.com" "pass1"
  friends <- getFriendsByStatusHandler conn 1 0
  assertEqual "Should return empty friends list" 0 (length friends)
  close conn

-- Test getFriendsByStatusHandler with pending friend requests
test_getFriendsByStatusHandler_pending :: Test
test_getFriendsByStatusHandler_pending = TestCase $ do
  conn <- testDB
  -- Create two users
  createTestUser conn "user1" "user1@example.com" "pass1"
  createTestUser conn "user2" "user2@example.com" "pass2"
  
  -- Create a friend request (status 0 = pending)
  _ <- createFriendRequestHandler conn 1 "user2"
  
  -- Get pending requests for user 2 (receiver)
  friends <- getFriendsByStatusHandler conn 2 0
  assertBool "Should have at least one pending request" (length friends > 0)
  close conn

-- Test acceptFriendRequestHandler success
test_acceptFriendRequestHandler_success :: Test
test_acceptFriendRequestHandler_success = TestCase $ do
  conn <- testDB
  -- Create two users
  createTestUser conn "user1" "user1@example.com" "pass1"
  createTestUser conn "user2" "user2@example.com" "pass2"
  
  -- Create a friend request first
  result1 <- createFriendRequestHandler conn 1 "user2"
  case result1 of
    Left err -> assertFailure $ "Failed to create friend request: " ++ T.unpack err
    Right _ -> do
      -- Accept the friend request
      result2 <- acceptFriendRequestHandler conn 2 1  -- user2 accepts user1's request
      case result2 of
        Left err -> assertFailure $ "Expected success, got error: " ++ T.unpack err
        Right successMsg -> assertEqual "Should return success message" "Friend request accepted" successMsg
  close conn

-- Test acceptFriendRequestHandler with non-existent request
test_acceptFriendRequestHandler_notfound :: Test
test_acceptFriendRequestHandler_notfound = TestCase $ do
  conn <- testDB
  createTestUser conn "user1" "user1@example.com" "pass1"
  
  -- Try to accept non-existent friend request
  result <- acceptFriendRequestHandler conn 1 999  -- non-existent user
  case result of
    Left err -> assertEqual "Should return failure message" "Failed to accept friend request" err
    Right _ -> assertFailure "Expected failure for non-existent request"
  close conn

-- Test refuseFriendRequestHandler success
test_refuseFriendRequestHandler_success :: Test
test_refuseFriendRequestHandler_success = TestCase $ do
  conn <- testDB
  -- Create two users
  createTestUser conn "user1" "user1@example.com" "pass1"
  createTestUser conn "user2" "user2@example.com" "pass2"
  
  -- Create a friend request first
  result1 <- createFriendRequestHandler conn 1 "user2"
  case result1 of
    Left err -> assertFailure $ "Failed to create friend request: " ++ T.unpack err
    Right _ -> do
      -- Refuse the friend request
      result2 <- refuseFriendRequestHandler conn 2 1  -- user2 refuses user1's request
      case result2 of
        Left err -> assertFailure $ "Expected success, got error: " ++ T.unpack err
        Right successMsg -> assertEqual "Should return success message" "Friend request refused" successMsg
  close conn

-- Test refuseFriendRequestHandler with non-existent request
test_refuseFriendRequestHandler_notfound :: Test
test_refuseFriendRequestHandler_notfound = TestCase $ do
  conn <- testDB
  createTestUser conn "user1" "user1@example.com" "pass1"
  
  -- Try to refuse non-existent friend request
  result <- refuseFriendRequestHandler conn 1 999  -- non-existent user
  case result of
    Left err -> assertEqual "Should return failure message" "Failed to refuse friend request" err
    Right _ -> assertFailure "Expected failure for non-existent request"
  close conn

-- Test getFriendsByStatusHandler with accepted friends
test_getFriendsByStatusHandler_accepted :: Test
test_getFriendsByStatusHandler_accepted = TestCase $ do
  conn <- testDB
  -- Create two users
  createTestUser conn "user1" "user1@example.com" "pass1"
  createTestUser conn "user2" "user2@example.com" "pass2"
  
  -- Create and accept a friend request
  result1 <- createFriendRequestHandler conn 1 "user2"
  case result1 of
    Left err -> assertFailure $ "Failed to create friend request: " ++ T.unpack err
    Right _ -> do
      result2 <- acceptFriendRequestHandler conn 2 1
      case result2 of
        Left err -> assertFailure $ "Failed to accept friend request: " ++ T.unpack err
        Right _ -> do
          -- Get accepted friends (status 1 = accepted)
          friends <- getFriendsByStatusHandler conn 1 1
          assertBool "Should have at least one accepted friend" (length friends > 0)
  close conn

-- Test getImageHandler with empty image ID
test_getImageHandler_empty :: Test
test_getImageHandler_empty = TestCase $ do
  result <- getImageHandler ""
  case result of
    Left err -> assertEqual "Should return bad request" "Bad request." err
    Right _ -> assertFailure "Expected failure for empty image ID"

-- Test getImageHandler with non-existent image
test_getImageHandler_notfound :: Test
test_getImageHandler_notfound = TestCase $ do
  result <- getImageHandler "nonexistent.jpg"
  case result of
    Left err -> assertEqual "Should return image not found" "Image not found." err
    Right _ -> assertFailure "Expected failure for non-existent image"

-- Test getImageHandler with existing image (creates a test file)
test_getImageHandler_success :: Test
test_getImageHandler_success = TestCase $ do
  -- Create images directory and test file
  createDirectoryIfMissing True "images"
  let testImagePath = "images/test_image.jpg"
      testImageId = "test_image.jpg"
  
  -- Create a test file
  writeFile testImagePath "test image content"
  
  -- Test the handler
  result <- getImageHandler (TL.pack testImageId)
  case result of
    Left err -> do
      -- Clean up even if test fails
      fileExists <- doesFileExist testImagePath
      if fileExists then removeFile testImagePath else return ()
      assertFailure $ "Expected success, got error: " ++ T.unpack err
    Right filePath -> do
      -- Should return the correct file path
      assertEqual "Should return correct file path" testImagePath filePath
      -- Clean up test file
      fileExists <- doesFileExist testImagePath
      if fileExists then removeFile testImagePath else return ()

-- Test uploadImageHandler with no files (empty list)
test_uploadImageHandler_empty :: Test
test_uploadImageHandler_empty = TestCase $ do
  result <- uploadImageHandler []
  case result of
    Left err -> assertEqual "Should return bad request" "Bad request" err
    Right _ -> assertFailure "Expected failure for empty file list"

-- Test deleteImageHandler with empty path
test_deleteImageHandler_empty :: Test
test_deleteImageHandler_empty = TestCase $ do
  result <- deleteImageHandler ""
  case result of
    Left err -> assertEqual "Should return bad request" "Bad request." err
    Right _ -> assertFailure "Expected failure for empty path"

-- Test deleteImageHandler with non-existent file
test_deleteImageHandler_notfound :: Test
test_deleteImageHandler_notfound = TestCase $ do
  result <- deleteImageHandler "nonexistent.jpg"
  case result of
    Left err -> assertEqual "Should return image not found" "Image not found." err
    Right _ -> assertFailure "Expected failure for non-existent file"

-- Test deleteImageHandler with existing file (creates and deletes a test file)
test_deleteImageHandler_success :: Test
test_deleteImageHandler_success = TestCase $ do
  -- Create images directory and test file
  createDirectoryIfMissing True "images"
  let testImagePath = "images/test_delete_image.jpg"
      testImageId = "test_delete_image.jpg"
  
  -- Create a test file
  writeFile testImagePath "test image content for deletion"
  
  -- Verify file exists before deletion
  fileExistsBefore <- doesFileExist testImagePath
  assertBool "Test file should exist before deletion" fileExistsBefore
  
  -- Test the delete handler
  result <- deleteImageHandler (TL.pack testImageId)
  case result of
    Left err -> do
      -- Clean up if test fails
      fileExists <- doesFileExist testImagePath
      if fileExists then removeFile testImagePath else return ()
      assertFailure $ "Expected success, got error: " ++ T.unpack err
    Right successMsg -> do
      -- Should return success message
      assertEqual "Should return success message" "Image deleted successfully." successMsg
      
      -- Verify file no longer exists
      fileExistsAfter <- doesFileExist testImagePath
      assertBool "Test file should not exist after deletion" (not fileExistsAfter)

-- Main test suite
tests :: Test
tests = TestList
  [ TestLabel "getUserHandler success" test_getUserHandler_success
  , TestLabel "getUserHandler not found" test_getUserHandler_notfound
  , TestLabel "getUserLoginHandler success" test_getUserLoginHandler_success
  , TestLabel "getUserLoginHandler invalid" test_getUserLoginHandler_invalid
  , TestLabel "editUserHandler success" test_editUserHandler_success
  , TestLabel "editUserHandler failure" test_editUserHandler_failure
  , TestLabel "userSignupHandler success" test_userSignupHandler_success
  , TestLabel "userSignupHandler duplicate email" test_userSignupHandler_duplicate_email
  , TestLabel "userSignupHandler duplicate username" test_userSignupHandler_duplicate_username
  , TestLabel "createFriendRequestHandler success" test_createFriendRequestHandler_success
  , TestLabel "createFriendRequestHandler not found" test_createFriendRequestHandler_notfound
  , TestLabel "getFriendsByStatusHandler no friends" test_getFriendsByStatusHandler_no_friends
  , TestLabel "getFriendsByStatusHandler pending" test_getFriendsByStatusHandler_pending
  , TestLabel "getFriendsByStatusHandler accepted" test_getFriendsByStatusHandler_accepted
  , TestLabel "acceptFriendRequestHandler success" test_acceptFriendRequestHandler_success
  , TestLabel "acceptFriendRequestHandler not found" test_acceptFriendRequestHandler_notfound
  , TestLabel "refuseFriendRequestHandler success" test_refuseFriendRequestHandler_success
  , TestLabel "refuseFriendRequestHandler not found" test_refuseFriendRequestHandler_notfound
  , TestLabel "createFriendRequestHandler self" test_createFriendRequestHandler_self
  , TestLabel "getImageHandler empty" test_getImageHandler_empty
  , TestLabel "getImageHandler not found" test_getImageHandler_notfound
  , TestLabel "getImageHandler success" test_getImageHandler_success
  , TestLabel "uploadImageHandler empty" test_uploadImageHandler_empty
  , TestLabel "deleteImageHandler empty" test_deleteImageHandler_empty
  , TestLabel "deleteImageHandler not found" test_deleteImageHandler_notfound
  , TestLabel "deleteImageHandler success" test_deleteImageHandler_success
  ]

-- Run all tests
main :: IO ()
main = do
  putStrLn "Running Handler Tests..."
  results <- runTestTT tests
  putStrLn $ "\nTests completed: " ++ show (tried results) ++ " run, " 
           ++ show (failures results) ++ " failures, " 
           ++ show (errors results) ++ " errors"
  return ()
