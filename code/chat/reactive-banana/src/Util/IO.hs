{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Util.IO (
    EventSource(..)
  , ExternalEventSource(..)
  , InternalEventSource(..)
  ) where

import           Control.Event.Handler      (AddHandler, newAddHandler)
import           Reactive.Banana            (Event)
import           Reactive.Banana.Frameworks (MomentIO, fromAddHandler, newEvent)

class EventSource e m | e -> m where
  mkEventSource :: m (e a)
  registerEvent :: e a -> MomentIO (Event a)
  fireEvent :: e a -> a -> IO ()

data ExternalEventSource a =
  ExternalEventSource {
    eeAddHandler :: AddHandler a
  , eeFire       :: a -> IO ()
  }

instance EventSource ExternalEventSource IO where
  mkEventSource =
    uncurry ExternalEventSource <$> newAddHandler
  registerEvent =
    fromAddHandler . eeAddHandler
  fireEvent =
    eeFire

data InternalEventSource a =
  InternalEventSource {
    ieEvent :: Event a
  , ieFire  :: a -> IO ()
  }

instance EventSource InternalEventSource MomentIO where
  mkEventSource =
    uncurry InternalEventSource <$> newEvent
  registerEvent =
    return . ieEvent
  fireEvent =
    ieFire

{-
data EventSource a =
  EventSource {
    addHandler :: AddHandler a
  , fire       :: a -> IO ()
  }

mkEventSource :: IO (EventSource a)
mkEventSource =
  uncurry EventSource <$> newAddHandler
-}
