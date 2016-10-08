{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Types.Message (
    Message
  , PrivateMessage(..)
  ) where

import qualified Data.Text as T

import Chat.Types.Name (Name)

type Message = T.Text

data PrivateMessage =
  PrivateMessage Name Message
  deriving (Eq, Ord, Show)
