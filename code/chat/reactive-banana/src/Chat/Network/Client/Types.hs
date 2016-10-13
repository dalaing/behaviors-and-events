{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Chat.Network.Client.Types (
    InputIO(..)
  , OutputIO(..)
  , InputSources(..)
  , mkInputSources
  ) where

import Data.IORef (IORef, newIORef)

import qualified Data.Text                  as T

import           Reactive.Banana            (Event)

import           Util.IO                    (EventSource (..))

data InputIO = InputIO {
    ieOpen  :: Event ()
  , ieRead  :: Event T.Text
  , ieClose :: Event ()
  }

data OutputIO = OutputIO {
    oeWrite :: Event T.Text
  , oeClose :: Event ()
  }

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

