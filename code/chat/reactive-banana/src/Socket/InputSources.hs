{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Socket.InputSources (
    ServerInputSources(..)
  , mkServerInputSources
  , ClientInputSources(..)
  , mkClientInputSources
  ) where

import           Control.Concurrent.STM.TChan (TChan, newTChanIO)
import           Control.Monad.IO.Class       (liftIO)
import           System.IO                    (Handle)

import qualified Data.Text                    as T

import           Reactive.Banana.Frameworks   (MomentIO)

import           Util.IO                      (ExternalEventSource,
                                               InternalEventSource,
                                               mkEventSource)

data ServerInputSources =
  ServerInputSources {
    ioHandle  :: ExternalEventSource Handle
  }

mkServerInputSources :: IO ServerInputSources
mkServerInputSources =
  ServerInputSources <$>
    mkEventSource

data ClientInputSources =
  ClientInputSources {
    cioHasOpened :: InternalEventSource ()
  , cioRead      :: InternalEventSource T.Text
  , cioHasClosed :: InternalEventSource ()
  , cioClosed    :: TChan ()
  }

mkClientInputSources :: MomentIO ClientInputSources
mkClientInputSources =
  ClientInputSources <$>
    mkEventSource <*>
    mkEventSource <*>
    mkEventSource <*>
    liftIO newTChanIO
