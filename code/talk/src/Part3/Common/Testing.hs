{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
module Part3.Common.Testing (
    Testable(..)
  , Fannable(..)
  , Mergable(..)
  , testNetwork
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks

class MonadMoment m => Testable m where
  interpretEvents :: (Event a -> m (Event b)) -> [Maybe a] -> IO [Maybe b]

instance Testable Moment where
  interpretEvents = interpret

instance Testable MomentIO where
  interpretEvents = interpretFrameworks

class Fannable i where
  type ToFan i
  fanInput :: Testable m => Event (ToFan i) -> m i

class Mergable o where
  type Merged o
  mergeOutput :: Event () -> o -> Event [Merged o]

testNetwork :: (Testable m, Fannable i, Mergable o) => (i -> m o) -> [Maybe (ToFan i)] -> IO [Maybe [Merged o]]
testNetwork fn =
  interpretEvents $ \i -> do
    fi <- fanInput i
    o <- fn fi
    return $ mergeOutput (() <$ i) o
