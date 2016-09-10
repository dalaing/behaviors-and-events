{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command.Types (
    CommandInput(..)
  , CommandOutput(..)
  ) where


import qualified Data.Set as S

import Reactive.Banana

import Part4.Types
import Part4.Types.Notification

data CommandInput = CommandInput {
    cieOpen   :: Event String
  , cieRead   :: Event String
  , cieNotify :: Event Notification
  , cibNames  :: Behavior (S.Set User)
  , cibName   :: Behavior User
  }

data CommandOutput = CommandOutput {
    coeWrite  :: Event String
  , coeClose  :: Event ()
  , coeNotify :: Event Notification
  , coeKick   :: Event User
  }
