{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Chat.Types.Notification (
    NotificationType(..)
  , Notification(..)
  , notificationText
  ) where

import GHC.Generics (Generic)

import qualified Data.Text as T

import Data.Aeson (FromJSON, ToJSON)

import Chat.Types.Name (Name)

data NotificationType =
    Stream
  | Batch
  deriving (Eq, Ord, Show)

data Notification =
    NJoin Name
  | NMessage Name T.Text
  | NTell Name Name T.Text
  | NKick Name Name
  | NQuit Name
  | NDisconnected Name
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Notification
instance ToJSON Notification

notificationText :: Notification
                 -> T.Text
notificationText (NJoin user) =
  T.concat [user, " has joined"]
notificationText (NMessage user message) =
  T.concat ["<", user, ">: ", message]
notificationText (NTell userFrom userTo message) =
  T.concat ["*", userFrom, " -> ", userTo, "*: ", message]
notificationText (NKick kicker kickee) =
  T.concat [kicker, " has kicked ", kickee]
notificationText (NQuit user) =
  T.concat [user, " has quit"]
notificationText (NDisconnected user) =
  T.concat [user, " has disconnected"]
