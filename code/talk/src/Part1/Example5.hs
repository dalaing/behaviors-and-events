{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part1.Example5 (
    go_1_5
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
    eMessage = filterE (/= "/quit") eRead
    eHelp    = () <$ filterE (== "/help") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ fmap putStrLn . leftmost $ [
      "Hi" <$ eOpen
    , eMessage
    , "/help displays this message\n/quit exits the program" <$ eHelp
    , "Bye" <$ eQuit
    ]
  reactimate $ exitSuccess <$ eQuit

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

go_1_5 :: IO ()
go_1_5 = do
  input <- mkInputSources
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
