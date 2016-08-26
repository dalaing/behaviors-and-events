{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part1.Example1 (
    go_1_1
  ) where

import Control.Concurrent (threadDelay)
import Data.Foldable (traverse_)

import Reactive.Banana
import Reactive.Banana.Frameworks

data EventSource a = EventSource {
    addHandler :: AddHandler a
  , fire       :: a -> IO ()
  }

mkEventSource :: IO (EventSource a)
mkEventSource =
  uncurry EventSource <$> newAddHandler

multiple :: Int -> Event Int -> Event Int
multiple m =
  filterE (\x -> x `mod` m == 0)

importantWork :: Event Int -> Event String
importantWork eCount =
  let
    eFizz =
      "Fizz" <$ multiple 3 eCount
    eBuzz =
      "Buzz" <$ multiple 5 eCount
    eFizzBuzz =
      unionWith (\_ _ -> "FizzBuzz")
      eFizz
      eBuzz
  in
    eFizzBuzz

networkDescription :: EventSource Int -> MomentIO ()
networkDescription c = do
  eCount <- fromAddHandler . addHandler $ c

  let
    eWrite = importantWork eCount

  reactimate $ (\x -> putStrLn $ "count: " ++ show x) <$> eCount
  reactimate $ putStrLn <$> eWrite

networkDescription2 :: EventSource Int -> MomentIO ()
networkDescription2 c = do
  eCount <- fromAddHandler . addHandler $ c

  let
    eFizz =
      "Fizz" <$ multiple 3 eCount
    eBuzz =
      "Buzz" <$ multiple 5 eCount
    eWrite =
      unionWith (\_ _ -> "FizzBuzz")
      eFizz
      eBuzz

  reactimate $ (\x -> putStrLn $ "count: " ++ show x) <$> eCount
  reactimate $ putStrLn <$> eWrite

eventStep :: EventSource Int -> Int -> IO ()
eventStep e i = do
  fire e i
  threadDelay 1000000

eventLoop :: EventSource Int -> IO ()
eventLoop e =
  traverse_ (eventStep e) [0..]

go_1_1 :: IO ()
go_1_1 = do
  input <- mkEventSource
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
