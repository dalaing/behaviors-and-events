{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Network.Types.Domain (
    DomainInput(..)
  , DomainOutput(..)
  ) where

import qualified Data.Text               as T

import           Reactive.Banana         (Event)

import           Chat.Types.Message      (Message, PrivateMessage)
import           Chat.Types.Name         (Name)
import           Chat.Types.Notification (Notification)
import           Util.Switch             (Switch (..), switchAp)

{-
We're going to want a type family that has
  :: MonadMoment m => a => m DomainInput
probably some kind of type + method for event sources + creating them
and instances for LineInput and DomainInput

This is complicated by the fact that we need _two_ functions from
LineInput to DomainInput - one for each phase.

Maybe we paramaterise ClientInput and ClientOutput, and have a type
family from ClientInput i -> ClientOutput o?
-}

data DomainInput =
  DomainInput {
    dieOpen         :: Event ()
  , dieName         :: Event Name
  , dieMessage      :: Event Message
  , dieTell         :: Event PrivateMessage
  , dieKick         :: Event Name
  , dieFetch        :: Event ()
  , dieHelp         :: Event ()
  , dieQuit         :: Event ()
  , dieUnknown      :: Event T.Text
  , dieDisconnected :: Event ()
  }

instance Switch DomainInput where
  switch e ee =
    DomainInput <$>
      switchAp dieOpen e ee <*>
      switchAp dieName e ee <*>
      switchAp dieMessage e ee <*>
      switchAp dieTell e ee <*>
      switchAp dieKick e ee <*>
      switchAp dieFetch e ee <*>
      switchAp dieHelp e ee <*>
      switchAp dieQuit e ee <*>
      switchAp dieUnknown e ee <*>
      switchAp dieDisconnected e ee

data DomainOutput =
  DomainOutput {
    doeNotify   :: Event [Notification]
  , doeFeedback :: Event T.Text
  , doeClose    :: Event ()
  }

instance Switch DomainOutput where
  switch e ee =
    DomainOutput <$>
      switchAp doeNotify e ee <*>
      switchAp doeFeedback e ee <*>
      switchAp doeClose e ee
