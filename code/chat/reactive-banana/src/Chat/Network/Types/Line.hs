{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Network.Types.Line (
    LineInput(..)
  , LineOutput(..)
  ) where

import qualified Data.Text       as T

import           Reactive.Banana (Event)

import           Util.Switch     (Switch (..), switchAp)

data LineInput = LineInput {
    lieOpen  :: Event ()
  , lieRead  :: Event T.Text
  , lieClose :: Event ()
  }

instance Switch LineInput where
  switch e ee =
    LineInput <$>
      switchAp lieOpen e ee <*>
      switchAp lieRead e ee <*>
      switchAp lieClose e ee

data LineOutput = LineOutput {
    loeWrite :: Event T.Text
  , loeClose :: Event ()
  }

instance Switch LineOutput where
  switch e ee =
    LineOutput <$>
      switchAp loeWrite e ee <*>
      switchAp loeClose e ee
