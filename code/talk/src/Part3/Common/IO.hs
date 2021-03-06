{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Part3.Common.IO (
    InputSources
  , mkInputSources
  , InputIO(..)
  , OutputIO(..)
  , MonadMomentIO(..)
  , mkNetwork
  , mkGo
  , InputIOCmd(..)
  , OutputIOCmd(..)
  ) where

import Control.Monad (forever)
import System.Exit (exitSuccess)

import Control.Lens

import Reactive.Banana
import Reactive.Banana.Frameworks

import Part3.Common.Testing

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

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

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

class Monad m => MonadMomentIO m where
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

mkGo :: (InputSources -> MomentIO ()) -> IO ()
mkGo n = do
  input <- mkInputSources
  network <- compile $ n input
  actuate network
  eventLoop input

data InputIOCmd =
    IOOpen
  | IORead String
  deriving (Eq, Ord, Show)

makePrisms ''InputIOCmd

data OutputIOCmd =
    IOWrite String
  | IOClose
  deriving (Eq, Ord, Show)

makePrisms ''OutputIOCmd

instance Fannable () InputIOCmd where
  type Fanned () InputIOCmd = InputIO
  fanInput eIn =
    InputIO <$>
      fanE _IOOpen eIn <*>
      fanE _IORead eIn

instance Mergable () OutputIOCmd where
  type ToMerge () OutputIOCmd = OutputIO
  mergeOutput _ (OutputIO eWrite eClose) =
    unionWith combineResult
      (mergeE (pure ()) _IOWrite eWrite)
      (mergeE (pure ()) _IOClose eClose)
