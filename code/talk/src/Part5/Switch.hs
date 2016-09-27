{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part5.Switch (
    Switch(..)
  , switchAp
  , once
  ) where

import Reactive.Banana

class Switch a where
  switch :: MonadMoment m => a -> Event a -> m a

instance Switch (Event a) where
  switch e1 ee = do
    f1 <- stepper Just (const Nothing <$ ee)
    f2 <- stepper (const Nothing) (Just <$ ee)
    e2 <- switchE ee
    return $ unionWith (\_ x -> x) (filterJust $ f1 <@> e1) (filterJust $ f2 <@> e2)

instance Switch (Behavior a) where
  switch = switchB

instance Switch a => Switch (Moment a) where
  switch e ee = do
    m <- liftMoment e
    return $ switch m (observeE ee)

instance (Switch a, Switch b) => Switch (a, b) where
  switch e ee =
    (,) <$> switchAp fst e ee <*> switchAp snd e ee

switchAp :: (Switch b, MonadMoment m) => (a -> b) -> a -> Event a -> m b
switchAp f a e = switch (f a) (f <$> e)

once :: MonadMoment m => Event a -> m (Event a)
once e = switch e (never <$ e)


