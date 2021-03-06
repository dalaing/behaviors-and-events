{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Local (
    goLocal
  ) where

import           Reactive.Banana.Frameworks (actuate, compile)

import           Chat.Network.Types         (LineInputSources, mkLineInputSources)
import           Local.EventLoop            (eventLoop)
import           Local.Network              (network)
import           Util.IO                    (ExternalEventSource)

goLocal :: IO ()
goLocal = do
  io <- mkLineInputSources :: IO (LineInputSources ExternalEventSource)
  nd <- compile $ network io
  actuate nd
  eventLoop io
