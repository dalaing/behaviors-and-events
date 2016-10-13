{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Util (
    leftmost
  , bulkRemove
  ) where

import qualified Data.Set as S
import qualified Data.Map as M

import Reactive.Banana (Event, unionWith, never)

leftmost :: [Event a]
         -> Event a
leftmost =
  foldl (unionWith const) never

bulkRemove :: Ord a => S.Set a -> M.Map a b -> M.Map a b
bulkRemove s m = S.foldr M.delete m s
