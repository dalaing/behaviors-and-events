{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command.Components.Message (
    MessageInput(..)
  , MessageOutput(..)
  , handleMessage
  ) where

import           Reactive.Banana

import           Part4.Types
import           Part4.Types.Notification

data MessageInput = MessageInput {
    mibName    :: Behavior User
  , mieMessage :: Event String
  }

data MessageOutput = MessageOutput {
    moeNotify :: Event Notification
  }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput bName eMessage) = do
  let
    eNotify = NMessage <$> bName <@> eMessage
  return $ MessageOutput eNotify
