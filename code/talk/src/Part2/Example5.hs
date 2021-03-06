{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Example5 (
    go_2_5
  ) where

import Control.Monad (forever)
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

data InputSources = InputSources {
    isOpen :: EventSource ()
  , isRead :: EventSource String
  }

mkInputSources :: IO InputSources
mkInputSources =
  InputSources <$> mkEventSource <*> mkEventSource

helpMessage :: String
helpMessage =
  "/help              - displays this message\n" ++
  "/quit              - exits the program"

networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage =  filterE ((/= "/") . take 1) eRead
    eHelp    =   () <$ filterE (== "/help") eRead
    eQuit    =   () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi"        <$ eOpen
      ,                eMessage
      , helpMessage <$ eHelp
      , "Bye"       <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

go_2_5 :: IO ()
go_2_5 = do
  input <- mkInputSources
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
