{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command (
    CommandInput(..)
  , CommandOutput(..)
  , handleCommand
  ) where

import           Reactive.Banana

import           Part4.Command.Domain
import           Part4.Command.Types
import           Part4.Common.Util
import           Part4.Types

handleCommand :: CommandInput -> Moment CommandOutput
handleCommand ci = do
  d <- pureCommandNetworkDescription . fanOut $ ci
  return $ fanIn d

