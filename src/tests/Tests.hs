{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Database.SQLite.Simple
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Exception (bracket)
import Data.Time.Calendar (fromGregorian)

import Database (connectDB, createTables, createUser)
import Handlers (
    getUserHandler,
    getUserLoginHandler, 
    userSignupHandler,
    createFriendRequestHandler,
    acceptFriendRequestHandler,
    refuseFriendRequestHandler,
    deleteImageHandler
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

-- Main test suite
tests :: Test
tests = TestList
  [ TestLabel "getUserHandler success" test_getUserHandler_success
  , TestLabel "getUserHandler not found" test_getUserHandler_notfound
  , TestLabel "getUserLoginHandler success" test_getUserLoginHandler_success
  , TestLabel "getUserLoginHandler invalid" test_getUserLoginHandler_invalid
  , TestLabel "userSignupHandler success" test_userSignupHandler_success
  , TestLabel "userSignupHandler duplicate email" test_userSignupHandler_duplicate_email
  , TestLabel "userSignupHandler duplicate username" test_userSignupHandler_duplicate_username
  , TestLabel "createFriendRequestHandler success" test_createFriendRequestHandler_success
  , TestLabel "createFriendRequestHandler not found" test_createFriendRequestHandler_notfound
  , TestLabel "createFriendRequestHandler self" test_createFriendRequestHandler_self
  , TestLabel "deleteImageHandler empty" test_deleteImageHandler_empty
  , TestLabel "deleteImageHandler not found" test_deleteImageHandler_notfound
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
