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

import           Local.EventLoop            (eventLoop)
import           Local.InputSources         (mkInputSources)
import           Local.Network              (network)

goLocal :: IO ()
goLocal = do
  io <- mkInputSources
  nd <- compile $ network io
  actuate nd
  eventLoop io
