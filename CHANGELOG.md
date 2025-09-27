# Changelog for `CMChatAPI`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- Added `Tests.hs` file to test all the necessary Handlers
- Added getImageHandler to `Handlers.hs`
- Added uploadImageHandler to `Handlers.hs`
- Added removeImageHandler to `Handlers.hs`

### Changed

- Changed error/logic treatment from every API endpoint functions to the corresponding handlers

### Removed

- Removed utils completely since they weren't being used
- Cleaned up some non used imports

## 0.1.1 - 2025-09-25

### Added

- Added `SocketServer.hs` file to manage everything WebSocket related
- Added real time messaging with WebSockets
- Added SocketMessage type to `Types.hs`
- Added Client type to `Types.hs`
- Added ServerState type to `Types.hs`
- Added ChatServer type to `Types.hs`
- Added refuseFriendRequest query in `Database.hs`
- Added acceptFriendRequest query in `Database.hs`
- Added refuseFriendRequestHandler function in `Handlers.hs`
- Added acceptFriendRequestHandler function in `Handlers.hs`
- Added getFriendByStatus function in `Handlers.hs`
- Added friends/ GET endpoint in `Api.hs` to get user friends by userId and status
- Added friend/request/refuce POST endpoint in `Api.hs` to refuse friend requests
- Added friend/request/accept POST endpoint in `Api.hs` to accept friend requests
- Added friend/request POST endpoint in `Api.hs` to send friend requests
- Added createFriendRequestHandler function in `Handlers.hs`
- Added getFriendsByStatus function in `Database.hs`
- Added getHaveFriendRequest function in `Database.hs`
- Added createFriendRequest function in `Database.hs`
- Added test queries for inserting friend data in the database
- Added getUserByUsername function in `Database.hs` to get user id by the username
- Added `Friend` Type data for sending friend information
- Added `FriendRequest` Type data for sending friend requests
- Added `friends` table to the database to handle friend requests
- Added editUser POST endpoint for editing user data
- Added editUser function in `Database.hs` to handle user editing
- Added editUserHandler function in `Handlers.hs`
- Added editUser type to `Types.hs`

### Changed

- Changed getFriendRequest to getHaveFriendRequest because its only a true/false value returned
- Changed table name `users` -> `user`
- Changed table name `friends` -> `friend`

### Removed

- Removed getFriendsRequestHandler

## 0.1.0.2 - 2025-09-17

### Added

- Added image GET endpoint for fetching images using their unique name
- Added image POST endpoint for uploading images
- Added image DELETE endpoint for deleting images
- Added new related dependencies to `.cabal` file

### Changed

- Reorganized imports in `Api.hs` and grouped them into categories

## 0.1.0.1 - 2025-09-17

### Changed

- Changed error / success messages for just message objects for compatibility with the application
- Changed queries related to login, from email authentication to username authentication

### Removed

- Removed unnecessary fields in NewUser

## 0.1.0.0 - 2025-09-17

### Added

- Main just calls the API startServer function
- Database for handling every database directly related action
- Handlers for making the bridge between the API and the database
- API to handle each endpoint and the data processing needed for the requests
- Types for defining each type needed for the API to work properly
- Utils for handling some functions
