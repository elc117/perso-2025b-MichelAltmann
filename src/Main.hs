{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Api (startServer)
import SocketServer (startSocketServer)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  putStrLn "Starting REST API server on port 3000..."
  putStrLn "Starting WebSocket server on port 9160..."
  
  -- Start WebSocket server in a separate thread
  _ <- forkIO startSocketServer
  
  -- Start REST API server (this will block)
  startServer