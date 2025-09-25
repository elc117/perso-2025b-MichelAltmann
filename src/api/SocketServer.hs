{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SocketServer (startSocketServer) where

import qualified Network.WebSockets as WS
import Control.Concurrent.STM
import Data.Aeson (decode, encode)
import Control.Monad (forever, forM_)
import Control.Exception (finally)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Random (randomRIO)
import Types (SocketMessage(..), Client(..), ChatServer(..))

-- Create a new chat server
newChatServer :: IO ChatServer
newChatServer = do
    clientsRef <- newTVarIO Map.empty
    return ChatServer { clients = clientsRef }

-- Add client to server
addClient :: ChatServer -> Int -> Client -> STM ()
addClient server userId client = do
    modifyTVar (clients server) (Map.insert userId client)

-- Remove client from server
removeClient :: ChatServer -> Int -> STM ()
removeClient server userId = do
    modifyTVar (clients server) (Map.delete userId)

-- Get all connected user IDs
getConnectedUsers :: ChatServer -> STM [Int]
getConnectedUsers server = do
    clientsMap <- readTVar (clients server)
    return (Map.keys clientsMap)

-- Broadcast to all connected users
broadcastToAll :: ChatServer -> SocketMessage -> IO ()
broadcastToAll server message = do
    clientsMap <- readTVarIO (clients server)
    let messageBS = encode message
    forM_ (Map.elems clientsMap) $ \client -> do
        WS.sendTextData (clientConn client) messageBS

-- Send message to specific client
sendToClient :: ChatServer -> Int -> SocketMessage -> IO ()
sendToClient server userId message = do
    clientsMap <- readTVarIO (clients server)
    case Map.lookup userId clientsMap of
        Just client -> WS.sendTextData (clientConn client) (encode message)
        Nothing -> putStrLn $ "Client " ++ show userId ++ " not found"

-- Handle incoming WebSocket messages
handleMessage :: ChatServer -> Int -> SocketMessage -> IO ()
handleMessage server fromUserId msg = case msg of
    SendMessage senderId' receiverId' senderName' senderImage' text' image' -> do
        putStrLn $ "Message from user " ++ show senderId' ++ " to user " ++ show receiverId' ++ ": " ++ T.unpack text'
        currentTime <- getCurrentTime
        let timeStr = T.pack $ show currentTime
        -- Generate random message ID for in-memory operation
        messageId' <- randomRIO (1000, 9999)
        
        -- Create response with "sent" status for sender
        let senderResponse = MessageReceived 
                { messageId = messageId'
                , senderId = senderId'
                , receiverId = receiverId'
                , senderName = senderName'
                , senderImage = senderImage'
                , text = text'
                , image = image'
                , status = "1"  -- sent
                , timestamp = timeStr
                }
        
        -- Create response with "delivered" status for receiver (if online)
        let receiverResponse = senderResponse { status = "2" }  -- delivered
        
        -- Send to recipient if they're online
        sendToClient server receiverId' receiverResponse
        -- Send confirmation back to sender
        sendToClient server senderId' senderResponse
    
    MessageStatusUpdate messageId' newStatus -> do
        putStrLn $ "Status update for message " ++ show messageId' ++ " to status " ++ T.unpack newStatus
        -- In-memory operation: status changes are handled per-message, no persistence needed
        -- Could notify sender about status change if needed
        
    _ -> do
        putStrLn $ "Unhandled message: " ++ show msg
        sendToClient server fromUserId (Error "Unknown message type")

-- WebSocket application
wsApp :: ChatServer -> WS.ServerApp
wsApp server pending = do
    -- Accept the WebSocket connection
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        -- Get user ID from the first message
        putStrLn "New WebSocket connection established"
        
        -- Wait for initial connection message with user ID
        initialMsg <- WS.receiveData conn
        case decode initialMsg of
            Just (UserConnect userId') -> do
                putStrLn $ "User " ++ show userId' ++ " connected"
                let client = Client conn userId'
                atomically $ addClient server userId' client
                
                -- Notify all users that this user is online
                broadcastToAll server (UserOnline userId')
                
                -- Handle subsequent messages
                flip finally (do
                    atomically $ removeClient server userId'
                    broadcastToAll server (UserOffline userId')
                    putStrLn $ "User " ++ show userId' ++ " disconnected"
                    ) $ do
                        forever $ do
                            msgData <- WS.receiveData conn
                            case decode msgData of
                                Just socketMsg -> handleMessage server userId' socketMsg
                                Nothing -> do
                                    putStrLn "Failed to parse WebSocket message"
                                    sendToClient server userId' (Error "Invalid message format")
            _ -> do
                WS.sendTextData conn (encode $ Error "First message must be UserConnect with userId")
                putStrLn "Connection rejected: invalid initial message"

-- Start the WebSocket server
startSocketServer :: IO ()
startSocketServer = do
    server <- newChatServer
    WS.runServer "0.0.0.0" 9160 (wsApp server)
