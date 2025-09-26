{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Database (connectDB, getUserById, createTables, createTestUser, getUserLogin, getEmail, createUser, getUsername, editUser, createFriendRequest,getFriendsByStatus, getUserIdByUsername, getHaveFriendRequest, acceptFriendRequest, refuseFriendRequest) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Types (EditUser (..), NewUser (..), User (..), FriendRequest (..), Friend (..))

-- Creating the database connection
connectDB :: IO Connection
connectDB = open "chatapp.db"

-- Creating tables if they don't exist
createTables :: Connection -> IO ()
createTables conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS user (\
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
    \deleted BOOLEAN NOT NULL DEFAULT FALSE\
    \);"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS friend (\
    \friendId INTEGER PRIMARY KEY AUTOINCREMENT,\
    \userId INTEGER NOT NULL,\
    \friendUserId INTEGER NOT NULL,\
    \status INTEGER NOT NULL,\
    \friendshipDate DATE NOT NULL DEFAULT (DATE('now')),\
    \FOREIGN KEY (userId) REFERENCES user (userId),\
    \FOREIGN KEY (friendUserId) REFERENCES user (userId)\
    \);" -- status 0=pending, 1=accepted, 2=rejected

-- Creating a test user
createTestUser :: Connection -> IO ()
createTestUser conn = do
  execute_
    conn
    "INSERT OR IGNORE INTO user (username, nickname, email, password, birthday, biography, profileImage, backgroundImage) \
    \VALUES ('Michel', 'Mijas', 'michel.altmann05@gmail.com', '12345', '2004-09-05', 'Biografia vai aqui', 'profile1.jpg', 'background1.jpg'),\
    \('Teste', 'Test', 'teste@gmail.com', '12345', '2004-09-05', 'Biografia vai aqui', 'profile2.jpg', 'background2.jpg'),\
    \('User3', 'User', 'user3@gmail.com', '12345', '2004-09-05', 'Biografia vai aqui', 'profile3.jpg', 'background3.jpg'),\
    \('User4', 'User', 'user4@gmail.com', '12345', '2004-09-05', 'Biografia vai aqui', 'profile4.jpg', 'background4.jpg');"

-- Getting user by ID
getUserById :: Connection -> Int -> IO (Maybe User)
getUserById conn userId = do
  rows <- query conn "SELECT * FROM user WHERE userId = ?" (Only userId)
  return $ case rows of
    [user] -> Just user
    _ -> Nothing

-- Getting user by email and password
getUserLogin :: Connection -> T.Text -> T.Text -> IO (Maybe User)
getUserLogin conn username password = do
  rows <- query conn "SELECT * FROM user WHERE username = ? AND password = ?" (username, password)
  return $ case rows of
    [user] -> Just user
    _ -> Nothing

-- Editing user by id
editUser :: Connection -> Int -> EditUser -> IO (Maybe EditUser)
editUser conn userId editUserData = do
  let EditUser _ uname nick email bday bio profImg bgImg = editUserData
  execute
    conn
    "UPDATE user SET username = ?, nickname = ?, birthday = ?, biography = ?, profileImage = ?, backgroundImage = ? WHERE userId = ?"
    ( uname,
      nick,
      bday,
      bio,
      profImg,
      bgImg,
      userId
    )
  n <- changes conn
  if n > 0
    then return (Just editUserData)
    else return Nothing

-- Getting email from database
getEmail :: Connection -> T.Text -> IO (Maybe T.Text)
getEmail conn email = do
  rows <- query conn "SELECT email FROM user WHERE email = ?" (Only email) :: IO [Only T.Text]
  return $ case rows of
    [Only e] -> Just e
    _ -> Nothing

-- Getting username from database
getUsername :: Connection -> T.Text -> IO (Maybe T.Text)
getUsername conn username = do
  rows <- query conn "SELECT username FROM user WHERE username = ?" (Only username) :: IO [Only T.Text]
  return $ case rows of
    [Only u] -> Just u
    _ -> Nothing

-- Getting userId using username
getUserIdByUsername :: Connection -> T.Text -> IO (Maybe Int)
getUserIdByUsername conn username = do
  rows <- query conn "SELECT userId FROM user WHERE username = ?" (Only username) :: IO [Only Int]
  return $ case rows of
    [Only uid] -> Just uid
    _ -> Nothing

-- Creating new user
createUser :: Connection -> NewUser -> IO Bool
createUser conn newUser = do
  execute
    conn
    "INSERT OR IGNORE INTO user (username, email, password, birthday) VALUES (?, ?, ?, ?)"
    newUser
  n <- changes conn
  return (n > 0)

-- Creating friend request (checks for existing friendship first)
createFriendRequest :: Connection -> Int -> Int -> IO Bool
createFriendRequest conn userId friendUserId = do
  -- Check if friendship already exists
  exists <- getHaveFriendRequest conn userId friendUserId
  if exists
    then return False  -- Friendship already exists
    else do
      execute
        conn
        "INSERT INTO friend (userId, friendUserId, status) VALUES (?, ?, 0)"
        (userId, friendUserId)
      n <- changes conn
      return (n > 0)

-- Getting friend requests for a user
getHaveFriendRequest :: Connection -> Int -> Int -> IO Bool
getHaveFriendRequest conn userId friendUserId = do
  rows <- query conn
    "SELECT COUNT(*) FROM friend WHERE (userId = ? AND friendUserId = ?) OR (friendUserId = ? AND userId = ?)"
    (userId, friendUserId, userId, friendUserId) :: IO [Only Int]
  case rows of
    [Only count] -> return (count > 0)
    _ -> return False

-- Getting friends list by status and userId
getFriendsByStatus :: Connection -> Int -> Int -> IO [Friend]
getFriendsByStatus conn userId status = do
  rows <- query conn
    "SELECT u.userId, u.username, u.nickname, u.email, u.birthday, u.biography, u.profileImage, u.backgroundImage, u.createdDate, u.deleted, f.status \
    \FROM user u \
    \INNER JOIN friend f ON (u.userId = f.friendUserId AND f.userId = ?) \
    \OR (u.userId = f.userId AND f.friendUserId = ?) \
    \WHERE u.userId != ? AND u.deleted = 0 AND f.status = ?"
    (userId, userId, userId, status)
  return rows

-- Accepting friend request (updating status to accepted)
acceptFriendRequest :: Connection -> Int -> Int -> IO Bool
acceptFriendRequest conn userId friendUserId = do
  execute
    conn
    "UPDATE friend SET status = 1 WHERE (userId = ? AND friendUserId = ?) OR (friendUserId = ? AND userId = ?)"
    (userId, friendUserId, userId, friendUserId)
  n <- changes conn
  return (n > 0)

-- Rejecting friend request (updating status to rejected)
refuseFriendRequest :: Connection -> Int -> Int -> IO Bool
refuseFriendRequest conn userId friendUserId = do
  execute
    conn
    "UPDATE friend SET status = 2 WHERE (userId = ? AND friendUserId = ?) OR (friendUserId = ? AND userId = ?)"
    (userId, friendUserId, userId, friendUserId)
  n <- changes conn
  return (n > 0)