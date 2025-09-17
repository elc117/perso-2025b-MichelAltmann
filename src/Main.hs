{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Api (startServer)

main :: IO ()
main = do
  putStrLn "Starting server on port 3000..."
  startServer