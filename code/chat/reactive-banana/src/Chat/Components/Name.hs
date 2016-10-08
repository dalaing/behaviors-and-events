{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Components.Name (
    NameInput(..)
  , NameOutput(..)
  ) where

import qualified Data.Set                as S
import qualified Data.Text               as T

import           Reactive.Banana         (Behavior, Event)

import           Chat.Types.Name         (Name)
import           Chat.Types.Notification (Notification)

data NameInput = NameInput {
    nibNames :: Behavior (S.Set Name)
  , nieOpen  :: Event ()
  , nieRead  :: Event T.Text
  }

data NameOutput = NameOutput {
    noeName   :: Event Name
  , noeNotify :: Event Notification
  , noeWrite  :: Event T.Text
  }
