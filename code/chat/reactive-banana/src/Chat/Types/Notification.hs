{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Types.Notification (
    Notification(..)
  , notificationText
  ) where

import qualified Data.Text as T

import Chat.Types.Name (Name)

data Notification =
    NJoin Name
  | NMessage Name T.Text
  | NTell Name Name T.Text
  | NKick Name Name
  | NQuit Name
  | NDisconnected Name
  deriving (Eq, Ord, Show)

notificationText :: Notification
                 -> T.Text
notificationText (NJoin user) =
  T.concat [user, " has joined"]
notificationText (NMessage user message) =
  T.concat ["<", user, ">: ", message]
notificationText (NTell userFrom _ message) =
  T.concat ["*", userFrom, "*: ", message]
notificationText (NKick kicker kickee) =
  T.concat [kicker, " has kicked ", kickee]
notificationText (NQuit user) =
  T.concat [user, " has quit"]
notificationText (NDisconnected user) =
  T.concat [user, " has disconnected"]
