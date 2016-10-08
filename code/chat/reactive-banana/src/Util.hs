{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Util (
    leftmost
  ) where

import Reactive.Banana (Event, unionWith, never)

leftmost :: [Event a]
         -> Event a
leftmost =
  foldl (unionWith const) never
