{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Part4.Common.Util (
    orElse
  , leftmost
  , filterPrism
  , stepperLens
  ) where

import Control.Lens
import Reactive.Banana

orElse :: Event a -> Event a -> Event a
orElse = unionWith const

leftmost :: [Event a] -> Event a
leftmost = foldl orElse never

filterPrism :: Prism' s a -> Event s -> Event a
filterPrism p e = filterJust $ preview p <$> e

stepperLens :: MonadMoment m => a -> Lens' s a -> Event s -> m (Behavior a)
stepperLens x l e = stepper x (view l <$> e)
