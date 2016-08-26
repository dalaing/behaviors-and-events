{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Common (
    InputIO(..)
  , OutputIO(..)
  , mkGo
  , leftmost
  ) where

import           Control.Monad              (forever)
import           System.Exit                (exitSuccess)

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

class MonadMoment m => MonadMomentIO m where
  toMomentIO :: m a -> MomentIO a

instance MonadMomentIO Moment where
  toMomentIO = liftMoment

instance MonadMomentIO MomentIO where
  toMomentIO = id

mkNetwork :: MonadMomentIO m => (InputIO -> m OutputIO) -> InputSources -> MomentIO ()
mkNetwork fn input = do
  i <- handleInput input
  o <- toMomentIO $ fn i
  handleOutput o

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

mkGo :: MonadMomentIO m => (InputIO -> m OutputIO) -> IO ()
mkGo n = do
  input <- mkInputSources
  network <- compile $ mkNetwork n input
  actuate network
  eventLoop input
