{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (User (..), Login (..), NewUser (..), EditUser (..), Friend (..), FriendRequest (..), SocketMessage (..), Client (..), ChatServer (..)) where

import qualified Network.WebSockets as WS
import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (Day)
import qualified Data.Map as Map
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)

-- User data type
data User = User
  { userId :: Int,
    username :: Text,
    nickname :: Maybe Text,
    email :: Text,
    password :: Text,
    birthday :: Day,
    biography :: Maybe Text,
    profileImage :: Maybe Text,
    backgroundImage :: Maybe Text,
    createdDate :: Day,
    deleted :: Bool
  }
  deriving (Show, Generic)

instance FromJSON User

-- Converting MySQL row to User
instance FromRow User where
  fromRow =
    User
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance ToJSON User

data NewUser = NewUser
  { username :: Text,
    email :: Text,
    password :: Text,
    birthday :: Day
  }
  deriving (Show, Generic)

instance FromJSON NewUser 

instance ToRow NewUser where
  toRow (NewUser u e p b) =
    [ toField u,
      toField e,
      toField p,
      toField b
    ]


-- Data type for editing user
data EditUser = EditUser
  { userId :: Int,
    username :: Text,
    nickname :: Maybe Text,
    email :: Text,
    birthday :: Day,
    biography :: Maybe Text,
    profileImage :: Maybe Text,
    backgroundImage :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON EditUser
instance ToJSON EditUser
instance ToRow EditUser where
  toRow (EditUser _ u n e b bio pi bi) =
    [ toField u,
      toField n,
      toField e,
      toField b,
      toField bio,
      toField pi,
      toField bi
    ]

-- Login request data type
data Login = Login
  { username :: Text,
    password :: Text
  }
  deriving (Show, Generic)

instance FromJSON Login

data FriendRequest = FriendRequest
  { userId :: Int,
    friendUserId :: Int,
    status :: Int,
    friendshipDate :: Day
  }
  deriving (Show, Generic)
instance FromRow FriendRequest where
  fromRow =
    FriendRequest
      <$> field
      <*> field
      <*> field
      <*> field
instance ToJSON FriendRequest

-- Friend combines User data with friendship status
data Friend = Friend
  { userId :: Int,
    username :: Text,
    nickname :: Maybe Text,
    email :: Text,
    birthday :: Day,
    biography :: Maybe Text,
    profileImage :: Maybe Text,
    backgroundImage :: Maybe Text,
    createdDate :: Day,
    deleted :: Bool,
    status :: Int  -- 0=pending, 1=accepted, 2=rejected
  }
  deriving (Show, Generic)

instance FromJSON Friend
instance ToJSON Friend

-- Converting MySQL row to Friend
instance FromRow Friend where
  fromRow =
    Friend
      <$> field  -- userId
      <*> field  -- username
      <*> field  -- nickname
      <*> field  -- email
      <*> field  -- birthday
      <*> field  -- biography
      <*> field  -- profileImage
      <*> field  -- backgroundImage
      <*> field  -- createdDate
      <*> field  -- deleted
      <*> field  -- friendshipStatus

-- WebSocket message types matching Android Message class
data SocketMessage 
    = UserConnect { userId :: Int }
    | SendMessage 
        { senderId :: Int
        , receiverId :: Int
        , senderName :: Maybe Text
        , senderImage :: Maybe Text  -- URL
        , text :: Text
        , image :: Maybe Text        -- Base64 encoded image data
        }
    | MessageReceived 
        { messageId :: Int
        , senderId :: Int
        , receiverId :: Int
        , senderName :: Maybe Text
        , senderImage :: Maybe Text
        , text :: Text
        , image :: Maybe Text
        , status :: Text             -- "0" pending, "1" sent, "2" delivered, "3" read
        , timestamp :: Text
        }
    | MessageStatusUpdate
        { messageId :: Int
        , status :: Text             -- Update message status to delivered/read
        }
    | UserOnline { userId :: Int }
    | UserOffline { userId :: Int }
    | Error { errorMessage :: Text }
    deriving (Show, Generic)

instance FromJSON SocketMessage
instance ToJSON SocketMessage

-- Client connection data
data Client = Client
    { clientConn :: WS.Connection
    , clientUserId :: Int
    }

-- Server state
type ServerState = Map.Map Int Client  -- userId -> Client

-- Chat server
data ChatServer = ChatServer
    { clients :: TVar ServerState
    }