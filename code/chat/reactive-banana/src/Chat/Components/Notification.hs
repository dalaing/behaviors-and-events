{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Components.Notification (
    NotifyInput(..)
  , NotifyOutput(..)
  , keywordFetch
  , parseFetch
  , helplineFetch
  ) where

import           Control.Monad           (guard)

import qualified Data.Text               as T

import           Reactive.Banana         (Behavior, Event)

import           Chat.Types.Config       (Config (..))
import           Chat.Types.Notification (Notification, NotificationType (..))

data NotifyInput = NotifyInput {
    nibLimit  :: Behavior Int
  , nieFetch  :: Event ()
  , nieNotify :: Event [Notification]
  }

data NotifyOutput = NotifyOutput {
    noeWrite :: Event T.Text
  }

keywordFetch :: Config -> Maybe T.Text
keywordFetch config
  | cNotificationType config == Batch =
    Just "fetch"
  | otherwise =
    Nothing

parseFetch :: Config
           -> T.Text
           -> Maybe ()
parseFetch c kw = do
  kwFetch <- keywordFetch c
  guard $ kw == kwFetch
  return ()

helplineFetch :: Config
              -> Maybe T.Text
helplineFetch config =
  case cNotificationType config of
    Batch ->
      Just "/fetch             - prints the pending notifications"
    _     ->
      Nothing
