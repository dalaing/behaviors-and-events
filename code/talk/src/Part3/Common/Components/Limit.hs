{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Common.Components.Limit (
    LimitInput(..)
  , LimitOutput(..)
  , handleLimit
  ) where

import Reactive.Banana

data LimitInput = LimitInput {
    lieLimitUp :: Event ()
  , lieLimitDown :: Event ()
  }

data LimitOutput = LimitOutput {
    loeLimit :: Event Int
  , lobLimit :: Behavior Int
  }

handleLimit :: MonadMoment m => LimitInput -> m LimitOutput
handleLimit (LimitInput eUp eDown) = do
  (eLimit, bLimit) <- mapAccum 1 . fmap (\f x -> (f x, f x)) . unions $ [
      (+ 1) <$ eUp
    , (max 0 . subtract 1) <$ eDown
    ]
  return $ LimitOutput eLimit bLimit
