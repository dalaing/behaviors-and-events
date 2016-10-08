{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Types.Config (
    Config(..)
  ) where

import           Chat.Types.Name         (NameType)
import           Chat.Types.Notification (NotificationType)

data Config = Config {
    cNameType         :: NameType
  , cNotificationType :: NotificationType
  }
