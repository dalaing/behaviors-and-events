{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command.Components.Open (
    OpenInput(..)
  , OpenOutput(..)
  , handleOpen
  ) where

import           Reactive.Banana

import           Part4.Types
import           Part4.Types.Notification

data OpenInput = OpenInput {
    oieOpen :: Event User
  }

data OpenOutput = OpenOutput {
    ooeNotify :: Event Notification
  }

handleOpen :: MonadMoment m => OpenInput -> m OpenOutput
handleOpen (OpenInput eOpen) = do
  let
    eNotify = NJoin <$> eOpen
  return $ OpenOutput eNotify
