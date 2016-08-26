{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Example1 (
    go_2_1
  ) where

import Control.Monad (forever)

import Reactive.Banana
import Reactive.Banana.Frameworks

data EventSource a = EventSource {
    addHandler :: AddHandler a
  , fire       :: a -> IO ()
  }

mkEventSource :: IO (EventSource a)
mkEventSource =
  uncurry EventSource <$> newAddHandler

setupInput :: IO (EventSource String)
setupInput =
  mkEventSource

networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eWrite = eRead

  reactimate $ putStrLn <$> eWrite

eventLoop :: EventSource String -> IO ()
eventLoop i =
  forever $ do
    x <- getLine
    fire i x

go_2_1 :: IO ()
go_2_1 = do
  input <- setupInput
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
