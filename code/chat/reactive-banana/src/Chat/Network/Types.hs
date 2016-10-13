{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Chat.Network.Types (
    InputIO(..)
  , OutputIO(..)
  , InputSources(..)
  , mkInputSources
  ) where

import Data.IORef (IORef, newIORef)

import qualified Data.Text                  as T

import           Reactive.Banana            (Event)

import           Util.IO                    (EventSource (..))
import Util.Switch (Switch(..), switchAp)

data InputIO = InputIO {
    ieOpen  :: Event ()
  , ieRead  :: Event T.Text
  , ieClose :: Event ()
  }

instance Switch InputIO where
  switch e ee =
    InputIO <$>
      switchAp ieOpen e ee <*>
      switchAp ieRead e ee <*>
      switchAp ieClose e ee

data OutputIO = OutputIO {
    oeWrite :: Event T.Text
  , oeClose :: Event ()
  }

instance Switch OutputIO where
  switch e ee =
    OutputIO <$>
      switchAp oeWrite e ee <*>
      switchAp oeClose e ee

data InputSources e =
  InputSources {
    ioHasOpened :: e ()
  , ioRead      :: e T.Text
  , ioHasClosed :: e ()
  , ioClosed    :: IORef Bool
  }

-- TODO generalize the creation of iorefs, so that
-- we don't need IO in the signature here
mkInputSources :: EventSource e IO => IO (InputSources e)
mkInputSources =
  InputSources <$>
    mkEventSource <*>
    mkEventSource <*>
    mkEventSource <*>
    newIORef False

