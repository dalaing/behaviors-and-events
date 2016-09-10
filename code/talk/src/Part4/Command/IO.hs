{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command.IO (
    IOInput(..)
  , IOOutput(..)
  ) where

import Reactive.Banana

data IOInput = IOInput {
    ioieOpen :: Event ()
  , ioieRead :: Event String
  }

data IOOutput = IOOutput {
    iooeWrite :: Event String
  , iooeClose :: Event ()
  }
