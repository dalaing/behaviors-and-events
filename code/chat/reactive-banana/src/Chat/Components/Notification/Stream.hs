{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Components.Notification.Stream (
    handleNotify
  ) where

import           Reactive.Banana              (MonadMoment)

import           Chat.Components.Notification (NotifyInput (..),
                                               NotifyOutput (..))
import           Chat.Types.Notification      (notificationText)

handleNotify :: MonadMoment m
             => NotifyInput
             -> m NotifyOutput
handleNotify (NotifyInput _ _ eNotify) =
  return . NotifyOutput $ notificationText <$> eNotify
