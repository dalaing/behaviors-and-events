{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Components.Notification.Batch (
    handleNotify
  ) where

import           Reactive.Banana               (MonadMoment, accumB, unions,
                                                (<@), (<@>))

import           Chat.Components.Notification  (NotifyInput (..),
                                                NotifyOutput (..))
import           Chat.Types.Notification       (notificationText)

handleNotify :: MonadMoment m
             => NotifyInput
             -> m NotifyOutput
handleNotify (NotifyInput bLimit eFetch eNotify) = do

  bMessages <- accumB [] . unions $ [
      (\n x -> take n . (x :)) <$> bLimit <@> eNotify
    , const [] <$ eFetch
    ]

  let
    gatherMessages = foldMap notificationText . reverse

  return . NotifyOutput $ gatherMessages <$> bMessages <@ eFetch
