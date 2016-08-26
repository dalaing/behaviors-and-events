{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Example6 (
    go_2_6
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

networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage = filterE ((/= "/") . take 1) eRead
    eCommand = fmap (drop 1) . filterE ((== "/") . take 1) $ eRead
    eHelp    = () <$ filterE (== "help") eCommand
    eQuit    = () <$ filterE (== "quit") eCommand

    commands        = ["help", "quit"]
    eUnknownCommand = filterE (`notElem` commands) eCommand

  reactimate $ fmap putStrLn . leftmost $ [
      "Hi (type /help for instructions)" <$ eOpen
    , eMessage
    , "/help displays this message\n/quit exits the program" <$ eHelp
    , "Bye" <$ eQuit
    , (\x -> "Unknown command: " ++ x ++ " (type /help for instructions)") <$> eUnknownCommand
    ]
  reactimate $ exitSuccess <$ eQuit

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

go_2_6 :: IO ()
go_2_6 = do
  input <- mkInputSources
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
