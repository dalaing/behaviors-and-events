{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Part4.Common.Testing (
    Testable(..)
  , Fannable(..)
  , Command(..)
  , cmdE
  , cmdB
  , fanE
  , fanB
  , Mergable(..)
  , Result(..)
  , combineResult
  , mergeE
  , mergeB
  , testNetwork
  ) where

import Control.Lens

import Reactive.Banana
import Reactive.Banana.Frameworks

import Part4.Common.Util

class MonadMoment m => Testable m where
  interpretEvents :: (Event a -> m (Event b)) -> [Maybe a] -> IO [Maybe b]

instance Testable Moment where
  interpretEvents = interpret

instance Testable MomentIO where
  interpretEvents = interpretFrameworks

{-
class Fannable i where
  type ToFan i
  fanInput :: Testable m => Event (ToFan i) -> m i

class Mergable o where
  type Merged o
  mergeOutput :: Event () -> o -> Event (Merged o)

testNetwork :: (Testable m, Fannable i, Mergable o) => (i -> m o) -> [Maybe (ToFan i)] -> IO [Maybe (Merged o)]
testNetwork fn =
  interpretEvents $ \i -> do
    fi <- fanInput i
    o <- fn fi
    return $ mergeOutput (() <$ i) o
-}

data Command b e = Command {
    _cmdB :: b
  , _cmdE :: e
  } deriving (Eq, Ord, Show)

makeLenses ''Command

class Fannable b e where
  type Fanned b e
  fanInput :: Testable m => Event (Command b e) -> m (Fanned b e)

fanE :: (Testable m, Fannable b e) => Prism' e a -> Event (Command b e) -> m (Event a)
fanE p = return . filterPrism p . fmap _cmdE

fanB :: (Testable m, Fannable b e) => a -> Lens' b a -> Event (Command b e) -> m (Behavior a)
fanB x l = stepperLens x (cmdB . l)

data Result b e = Result {
    _resB :: b
  , _resE :: [e]
  } deriving (Eq, Ord, Show)

makeLenses ''Result

combineResult :: Result b e -> Result b e -> Result b e
combineResult (Result b1 e1) (Result b2 e2) =
  Result b1 (e1 ++ e2)

class Mergable b e where
  type ToMerge b e
  mergeOutput :: Event () -> ToMerge b e -> Event (Result b e)

mergeE :: Mergable b e => Behavior b -> Prism' e a -> Event a -> Event (Result b e)
mergeE b p e =
  (\bv ev -> Result bv [review p ev]) <$> b <@> e

mergeB :: Mergable b e => Behavior b -> Event () -> Event (Result b e)
mergeB b e =
  (\bv -> Result bv []) <$> b <@ e

testNetwork :: (Testable m, Fannable ib ie, Mergable ob oe) => (Fanned ib ie -> m (ToMerge ob oe)) -> [Maybe (Command ib ie)] -> IO [Maybe (Result ob oe)]
testNetwork fn =
  interpretEvents $ \i -> do
    fi <- fanInput i
    o <- fn fi
    return $ mergeOutput (() <$ i) o
