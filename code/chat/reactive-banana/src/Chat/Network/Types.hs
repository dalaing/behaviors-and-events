{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Chat.Network.Types (
    LineInput(..)
  , LineOutput(..)
  , LineInputSources(..)
  , mkLineInputSources
  ) where

import Data.IORef (IORef, newIORef)

import qualified Data.Text                  as T

import           Reactive.Banana            (Event)

import           Util.IO                    (EventSource (..))
import           Util.Switch (Switch(..), switchAp)

data LineInput = LineInput {
    lieOpen  :: Event ()
  , lieRead  :: Event T.Text
  , lieClose :: Event ()
  }

instance Switch LineInput where
  switch e ee =
    LineInput <$>
      switchAp lieOpen e ee <*>
      switchAp lieRead e ee <*>
      switchAp lieClose e ee

data LineOutput = LineOutput {
    loeWrite :: Event T.Text
  , loeClose :: Event ()
  }

instance Switch LineOutput where
  switch e ee =
    LineOutput <$>
      switchAp loeWrite e ee <*>
      switchAp loeClose e ee

data LineInputSources e =
  LineInputSources {
    liesHasOpened :: e ()
  , liesRead      :: e T.Text
  , liesHasClosed :: e ()
  , liesClosed    :: IORef Bool
  }

-- TODO generalize the creation of iorefs, so that
-- we don't need IO in the signature here
mkLineInputSources :: EventSource e IO => IO (LineInputSources e)
mkLineInputSources =
  LineInputSources <$>
    mkEventSource <*>
    mkEventSource <*>
    mkEventSource <*>
    newIORef False

