{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command.Components.Kick (
    KickInput(..)
  , KickOutput(..)
  , handleKick
  ) where

import qualified Data.Set                 as S

import           Reactive.Banana

import           Part4.Common.Util
import           Part4.Types
import           Part4.Types.NameError
import           Part4.Types.Notification

data KickInput = KickInput {
    kibNames :: Behavior (S.Set User)
  , kibName  :: Behavior User
  , kieName  :: Event String
  }

data KickOutput = KickOutput {
    koeKickValid :: Event User
  , koeNotify    :: Event Notification
  , koeError     :: Event String
  }

checkNotSelf :: User -> User -> Either String User
checkNotSelf name target
  | name == target = Left "Stop kicking yourself"
  | otherwise      = Right target

handleKick :: MonadMoment m => KickInput -> m KickOutput
handleKick (KickInput bNames bName eName) = do
  let
    (eKickSelfError, eNotSelf) = split $ checkNotSelf <$> bName <@> eName
    (eNameInvalid, eNameValid) = split $ checkValidNameInUse <$> bNames <@> eNotSelf
    eNotify = NKick <$> bName <@> eNameValid
    eError = leftmost [
        eKickSelfError
      , nameErrorMessage <$> eNameInvalid
      ]
  return $ KickOutput eNameValid eNotify eError
