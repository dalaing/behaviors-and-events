{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Types.Notification (
    Notification(..)
  , notificationMessage
  , NotificationType(..)
  , NotificationInput(..)
  , NotificationOutput(..)
  , handleNotification
  ) where

import Data.List (intercalate)

import Reactive.Banana

import Part4.Types

data Notification =
    NJoin User
  | NMessage User Message
  | NTell User User Message
  | NKick User User
  | NQuit User
  deriving (Eq, Ord, Show)

notificationMessage :: Notification -> String
notificationMessage (NJoin user) =
  user ++ " has joined"
notificationMessage (NMessage user message) =
  "<" ++ user ++ ">: " ++ message
notificationMessage (NTell userFrom userTo message) =
  "*" ++ userFrom ++ "*: " ++ message
notificationMessage (NKick kicker kickee) =
  kicker ++ " has kicked " ++ kickee
notificationMessage (NQuit user) =
  user ++ " has quit"

data NotificationType =
    Stream
  | Batch (Behavior Int)

data NotificationInput = NotificationInput {
    nibName         :: Behavior User
  , nieFetch        :: Event ()
  , nieNotification :: Event Notification
  }

data NotificationOutput = NotificationOutput {
    noeNotifications :: Event String
  }

handleNotificationStream :: MonadMoment m => NotificationInput -> m (Event [Notification])
handleNotificationStream (NotificationInput _ _ eNotify) =
  return $ pure <$> eNotify

addToBoundedList :: Int -> a -> [a] -> [a]
addToBoundedList limit x xs =
  take limit (x : xs)

handleNotificationBatch :: MonadMoment m => Behavior Int -> NotificationInput -> m (Event [Notification])
handleNotificationBatch bLimit (NotificationInput _ eFetch eNotify) = do
  bNotifications <- accumB [] . unions $ [
      addToBoundedList <$> bLimit <@> eNotify
    , const [] <$ eFetch
    ]
  return $ reverse <$> bNotifications <@ eFetch

filterPrivate :: User -> Notification -> Maybe Notification
filterPrivate name t@(NTell _ target _)
  | name == target = Just t
  | otherwise      = Nothing
filterPrivate _ x =
  Just x

prepareNotificationInput :: NotificationInput -> NotificationInput
prepareNotificationInput (NotificationInput bName eFetch eNotify) =
    NotificationInput bName eFetch eNotify'
  where
    eNotify' = filterJust $ filterPrivate <$> bName <@> eNotify

prepareNotificationOutput :: Event [Notification] -> NotificationOutput
prepareNotificationOutput eNotifications =
  NotificationOutput $ (intercalate "\n" . fmap notificationMessage) <$> eNotifications

handleNotification :: MonadMoment m => NotificationType -> NotificationInput -> m NotificationOutput
handleNotification Stream =
  fmap prepareNotificationOutput .
  handleNotificationStream .
  prepareNotificationInput
handleNotification (Batch limit) =
  fmap prepareNotificationOutput .
  handleNotificationBatch limit .
  prepareNotificationInput
