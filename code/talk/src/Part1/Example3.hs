{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part1.Example3 (
    go_1_3
  , myTestableNetwork
  , Input(..)
  , Output(..)
  , example1
  ) where

import Control.Monad (forever)
import Data.Foldable (traverse_)
import System.Exit (exitSuccess)

import Reactive.Banana
import Reactive.Banana.Frameworks

orElse :: Event a -> Event a -> Event a
orElse = unionWith const

leftmost :: [Event a] -> Event a
leftmost = foldl orElse never

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

data Input =
  Read String
  deriving (Eq, Ord, Show)

inputToRead :: Event Input -> Event String
inputToRead eIn =
    filterJust $ maybeRead <$> eIn
  where
    maybeRead (Read x) = Just x
    maybeRead _ = Nothing

data Output =
    Write String
  | Close
  deriving (Eq, Ord, Show)

handleInput :: EventSource String -> MomentIO (Event Input)
handleInput i = do
  eRead <- fromAddHandler . addHandler $ i
  return $ Read <$> eRead

myTestableNetwork :: Event Input -> Moment (Event [Output])
myTestableNetwork eIn =
  let
    eRead    = inputToRead eIn
    eMessage = filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite   = leftmost [
        eMessage
      , "Bye" <$ eQuit
      ]
    eOut = fmap ($ []) . unions $ [
        (\x xs -> Write x : xs) <$> eWrite
      , (Close :) <$ eQuit
      ]
  in
    return eOut

example1 :: [Maybe Input]
example1 = [Just (Read "one"), Nothing, Just (Read "two"), Just (Read "/quit")]

handleOutput :: Output -> IO ()
handleOutput (Write s) = putStrLn s
handleOutput Close = exitSuccess

networkDescription :: EventSource String -> MomentIO ()
networkDescription s = do
  i <- handleInput s
  o <- liftMoment $ myTestableNetwork i
  reactimate $ traverse_ handleOutput <$> o

eventLoop :: EventSource String -> IO ()
eventLoop s =
  forever $ do
    x <- getLine
    fire s x

go_1_3 :: IO ()
go_1_3 = do
  input <- setupInput
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
