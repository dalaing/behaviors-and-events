{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Types.Config (
    NotificationType(..)
  , Config(..)
  ) where

-- TODO move this to Chat.Types.Notification
data NotificationType =
    Stream
  | Batch
  deriving (Eq, Ord, Show)

data Config = Config {
    cNotificationType :: NotificationType
  }
