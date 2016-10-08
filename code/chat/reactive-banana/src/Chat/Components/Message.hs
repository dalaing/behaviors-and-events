{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Components.Message (
    MessageInput(..)
  , MessageOutput(..)
  , helplineMessage
  , handleMessage
  ) where

import qualified Data.Text               as T

import           Reactive.Banana         (Behavior, Event, MonadMoment, filterE,
                                          (<@>))

import           Chat.Types.Name         (Name)
import           Chat.Types.Message     (Message)
import           Chat.Types.Notification (Notification (..))

data MessageInput = MessageInput {
    mibName    :: Behavior Name
  , mieMessage :: Event Message
  }

data MessageOutput = MessageOutput {
    moeNotify :: Event Notification
  }

helplineMessage :: T.Text
helplineMessage =
  "<msg>              - sends a message to all users"

handleMessage :: MonadMoment m
              => MessageInput
              -> m MessageOutput
handleMessage (MessageInput bName eMessage) =
  let
    eNonEmptyMessage = filterE (not . T.null) eMessage
    eNotify = NMessage <$> bName <@> eNonEmptyMessage
  in
    return $ MessageOutput eNotify
