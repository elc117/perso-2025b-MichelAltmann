{-# LANGUAGE OverloadedStrings #-}


module Database (connectDB, getUserById, createTables, createTestUser, getUserLogin, getEmail, createUser, getUsername) where

import qualified Data.Text as T
import Data.Time (Day)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Types (NewUser (..), User (..))

-- Creating the database connection
connectDB :: IO Connection
connectDB = open "chatapp.db"

-- Creating tables if they don't exist
createTables :: Connection -> IO ()
createTables conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS users (\
    \userId INTEGER PRIMARY KEY AUTOINCREMENT,\
    \username TEXT NOT NULL UNIQUE,\
    \nickname TEXT,\
    \email TEXT NOT NULL UNIQUE,\
    \password TEXT NOT NULL,\
    \birthday DATE NOT NULL,\
    \biography TEXT,\
    \profileImage TEXT,\
    \backgroundImage TEXT,\
    \createdDate DATE NOT NULL DEFAULT (DATE('now')),\
    \deleted BOOLEAN NOT NULL DEFAULT 0\
    \);"

-- Creating a test user
createTestUser :: Connection -> IO ()
createTestUser conn =
  execute_
    conn
    "INSERT OR IGNORE INTO users (username, nickname, email, password, birthday, biography, profileImage, backgroundImage) \
    \VALUES ('Michel', 'Mijas', 'michel.altmann05@gmail.com', '12345', '2004-09-05', 'Biografia vai aqui', '2025-09-14', 0);"

-- Getting user by ID
getUserById :: Connection -> Int -> IO (Maybe User)
getUserById conn userId = do
  rows <- query conn "SELECT * FROM users WHERE userId = ?" (Only userId)
  return $ case rows of
    [user] -> Just user
    _ -> Nothing

-- Getting user by email and password
getUserLogin :: Connection -> T.Text -> T.Text -> IO (Maybe User)
getUserLogin conn username password = do
  rows <- query conn "SELECT * FROM users WHERE username = ? AND password = ?" (username, password)
  return $ case rows of
    [user] -> Just user
    _ -> Nothing

-- Getting email from database
getEmail :: Connection -> T.Text -> IO (Maybe T.Text)
getEmail conn email = do
  rows <- query conn "SELECT email FROM users WHERE email = ?" (Only email) :: IO [Only T.Text]
  return $ case rows of
    [Only e] -> Just e
    _ -> Nothing

-- Getting username from database
getUsername :: Connection -> T.Text -> IO (Maybe T.Text)
getUsername conn username = do
  rows <- query conn "SELECT username FROM users WHERE username = ?" (Only username) :: IO [Only T.Text]
  return $ case rows of
    [Only u] -> Just u
    _ -> Nothing

-- Creating new user
createUser :: Connection -> NewUser -> IO Bool
createUser conn newUser = do
  execute conn
    "INSERT OR IGNORE INTO users (username, email, password, birthday) VALUES (?, ?, ?, ?)"
    newUser
  n <- changes conn
  return (n > 0)