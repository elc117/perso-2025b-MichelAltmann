# Changelog for `CMChatAPI`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- Added friend/request POST endpoint in `Api.hs` to send friend requests
- Added createFriendRequestHandler function in `Handlers.hs`
- Added getFriendsByStatus function in `Database.hs`
- Added getFriendRequest function in `Database.hs`
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
