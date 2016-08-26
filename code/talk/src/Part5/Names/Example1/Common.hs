{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part5.Names.Example1.Common (
    EventSource(..)
  , InputSources(..)
  , mkInputSources
  , InputIO(..)
  , OutputIO(..)
  , mkNetwork
  , InputCmd(..)
  , OutputCmd(..)
  , testNetwork
  , leftmost
  , Switch(..)
  , switchAp
  ) where

import           System.Exit                (exitSuccess)

import           Reactive.Banana
import           Reactive.Banana.Frameworks

orElse :: Event a -> Event a -> Event a
orElse = unionWith const

leftmost :: [Event a] -> Event a
leftmost = foldl orElse never

class Switch a where
  switch :: MonadMoment m => a -> Event a -> m a

switchAp :: (Switch b, MonadMoment m) => (a -> b) -> a -> Event a -> m b
switchAp f a e = switch (f a) (f <$> e)

instance Switch (Behavior a) where
  switch = switchB

instance Switch (Event a) where
  switch = switchE

instance Switch a => Switch (Moment a) where
  switch e ee = do
    m <- liftMoment e
    return $ switch m (observeE ee)

instance (Switch a, Switch b) => Switch (a, b) where
  switch e ee =
    (,) <$> switchAp fst e ee <*> switchAp snd e ee

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

instance Switch OutputIO where
  switch e ee =
    OutputIO <$>
      switchAp ioeWrite e ee <*>
      switchAp ioeClose e ee

handleOutput :: OutputIO -> MomentIO ()
handleOutput (OutputIO eWrite eClose) = do
  reactimate $ putStrLn <$> eWrite
  reactimate $ exitSuccess <$ eClose

mkNetwork :: (InputIO -> MomentIO OutputIO) -> InputSources -> MomentIO ()
mkNetwork fn input = do
  i <- handleInput input
  o <- fn i
  handleOutput o

data InputCmd =
    Open
  | Read String
  deriving (Eq, Ord, Show)

fanInput :: Event InputCmd -> InputIO
fanInput eIn =
  let
    maybeOpen Open = Just ()
    maybeOpen _    = Nothing
    eOpen = filterJust $ maybeOpen <$> eIn

    maybeRead (Read x) = Just x
    maybeRead _ = Nothing
    eRead = filterJust $ maybeRead <$> eIn
  in
    InputIO eOpen eRead

data OutputCmd =
    Write String
  | Close
  deriving (Eq, Ord, Show)

mergeOutput :: OutputIO -> Event [OutputCmd]
mergeOutput (OutputIO eWrite eClose) =
  unionWith (++)
    ((\x -> [Write x]) <$> eWrite)
    ([Close] <$ eClose)

testNetwork :: (InputIO -> Moment OutputIO) -> [Maybe InputCmd] -> IO [Maybe [OutputCmd]]
testNetwork fn =
  interpret $ \i -> do
    o <- fn . fanInput $ i
    return $ mergeOutput o
