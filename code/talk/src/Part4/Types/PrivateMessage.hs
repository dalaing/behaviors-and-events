{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Types.PrivateMessage (
    PrivateMessage(..)
  ) where

import Part4.Types

data PrivateMessage = PrivateMessage {
    pmTo      :: User
  , pmMessage :: Message
  } deriving (Eq, Ord, Show)
