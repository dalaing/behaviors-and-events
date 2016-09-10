{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command.Components.Quit (
    QuitInput(..)
  , QuitOutput(..)
  , handleQuit
  ) where

import           Reactive.Banana

import           Part4.Types
import           Part4.Types.Notification

data QuitInput = QuitInput {
    qibName :: Behavior User
  , qieQuit :: Event ()
  }

data QuitOutput = QuitOutput {
    qoeNotify :: Event Notification
  , qoeClose  :: Event ()
  }

handleQuit :: MonadMoment m => QuitInput -> m QuitOutput
handleQuit (QuitInput bName eQuit) = do
  let
    eNotify = NQuit <$> bName <@ eQuit
  return $ QuitOutput eNotify eQuit
