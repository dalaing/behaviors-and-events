{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Socket (
    goSocket
  , goSocket2
  ) where

import           Reactive.Banana.Frameworks (actuate, compile)

import           Socket.EventLoop            (serverEventLoop)
import           Socket.InputSources         (mkServerInputSources)
import           Socket.Network              (network, network2)

goSocket :: Int -> IO ()
goSocket port = do
  io <- mkServerInputSources
  nd <- compile $ network io
  actuate nd
  serverEventLoop port io

goSocket2 :: Int -> IO ()
goSocket2 port = do
  io <- mkServerInputSources
  nd <- compile $ network2 io
  actuate nd
  serverEventLoop port io
