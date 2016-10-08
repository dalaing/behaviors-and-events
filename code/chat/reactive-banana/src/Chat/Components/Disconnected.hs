{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Components.Disconnected (
    DisconnectedInput(..)
  , DisconnectedOutput(..)
  , handleDisconnected
  ) where

import           Reactive.Banana         (Behavior, Event, MonadMoment, (<@))

import           Chat.Types.Name         (Name)
import           Chat.Types.Notification (Notification (..))

data DisconnectedInput = DisconnectedInput {
    mibName         :: Behavior Name
  , mieDisconnected :: Event ()
  }

data DisconnectedOutput = DisconnectedOutput {
    moeNotify       :: Event Notification
  , moeDisconnected :: Event ()
  }

handleDisconnected :: MonadMoment m
                   => DisconnectedInput
                  -> m DisconnectedOutput
handleDisconnected (DisconnectedInput bName eDisconnected) =
  let
    eNotify = NDisconnected <$> bName <@ eDisconnected
  in
    return $ DisconnectedOutput eNotify eDisconnected
