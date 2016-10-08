{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Components.Quit (
    QuitInput(..)
  , QuitOutput(..)
  , keywordQuit
  , parseQuit
  , helplineQuit
  , handleQuit
  ) where

import qualified Data.Text               as T

import           Reactive.Banana         (Behavior, Event, MonadMoment, (<@))

import           Chat.Types.Name         (Name)
import           Chat.Types.Notification (Notification (..))

data QuitInput = QuitInput {
    mibName :: Behavior Name
  , mieQuit :: Event ()
  }

data QuitOutput = QuitOutput {
    moeNotify :: Event Notification
  , moeQuit   :: Event ()
  }

keywordQuit :: T.Text
keywordQuit =
  "quit"

parseQuit :: T.Text
          -> Maybe ()
parseQuit cmd
  | cmd == keywordQuit = Just ()
  | otherwise          = Nothing

helplineQuit :: T.Text
helplineQuit =
  "/quit              - exits the program"

handleQuit :: MonadMoment m
           => QuitInput
           -> m QuitOutput
handleQuit (QuitInput bName eQuit) =
  let
    eNotify = NQuit <$> bName <@ eQuit
  in
    return $ QuitOutput eNotify eQuit
