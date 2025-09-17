# Changelog for `CMChatAPI`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Changed

- Changed error / success messages for just message objects for compatibility with the application
- Changed queries related to login, from email authentication to username authentication

### Removed

- Removed unnecessary fields in NewUser

## 0.1.0.1 - 2025-09-17

### Added

- Main just calls the api startServer function
- Database for handling every database directly related action
- Handlers for making the bridge between the api and the database
- Api to handle each endpoint and the data processing needed for the requests
- Types for defining each type needed for the api to work properly
- Utils for handling some functions

## 0.1.0.0 - 2025-09-17
