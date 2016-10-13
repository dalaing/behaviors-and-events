{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Local.InputSources (
    InputSources(..)
  , mkInputSources
  ) where

import           Data.IORef (IORef, newIORef)

import qualified Data.Text  as T

import           Util.IO    (EventSource(..), ExternalEventSource)

data InputSources =
  InputSources {
    ioHasOpened :: ExternalEventSource ()
  , ioRead      :: ExternalEventSource T.Text
  , ioHasClosed :: ExternalEventSource ()
  , ioClosed    :: IORef Bool
  }

mkInputSources :: IO InputSources
mkInputSources =
  InputSources <$>
    mkEventSource <*>
    mkEventSource <*>
    mkEventSource <*>
    newIORef False
