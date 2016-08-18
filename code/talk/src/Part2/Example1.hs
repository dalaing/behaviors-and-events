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

import           Control.Monad              (forever)
import           Data.Foldable              (traverse_)
import           System.Exit                (exitSuccess)

import           Data.Profunctor

import           Reactive.Banana
import           Reactive.Banana.Frameworks

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

data InputIO = InputIO {
    ioeOpen :: Event ()
  , ioeRead :: Event String
  }

handleInput :: InputSources -> MomentIO InputIO
handleInput (InputSources iso isr) = do
  eOpen <- fromAddHandler . addHandler $ iso
  eRead <- fromAddHandler . addHandler $ isr
  return $ InputIO eOpen eRead

data OutputIO = OutputIO {
    ioeWrite :: Event String
  , ioeClose :: Event ()
  }

handleOutput :: OutputIO -> MomentIO ()
handleOutput (OutputIO eWrite eClose) = do
  reactimate $ putStrLn <$> eWrite
  reactimate $ exitSuccess <$ eClose

mkNetwork :: (InputIO -> Moment OutputIO) -> InputSources -> MomentIO ()
mkNetwork fn input = do
  i <- handleInput input
  o <- liftMoment $ fn i
  handleOutput o

pureNetworkDescription :: MonadMoment m => InputIO -> m OutputIO
pureNetworkDescription (InputIO eOpen eRead) =
  let
    eMessage = filterE ((/= "/") . take 1) eRead
    eCommand = fmap (drop 1) . filterE ((== "/") . take 1) $ eRead
    eHelp    = () <$ filterE (== "help") eCommand
    eQuit    = () <$ filterE (== "quit") eCommand

    commands        = ["help", "quit"]
    eUnknownCommand = filterE (`notElem` commands) eCommand

    eWrite = leftmost [
        "Hi (type /help for instructions)" <$ eOpen
      , eMessage
      , "/help displays this message\n/quit exits the program" <$ eHelp
      , "Bye" <$ eQuit
      , (\x -> "Unknown command: " ++ x ++ " (type /help for instructions)") <$> eUnknownCommand
      ]
  in
    return $ OutputIO eWrite eQuit

networkDescription :: InputSources -> MomentIO ()
networkDescription =
  mkNetwork pureNetworkDescription

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

go_2_1 :: IO ()
go_2_1 = do
  input <- mkInputSources
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
