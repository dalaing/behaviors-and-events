{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command.Components.Tell (
    TellInput(..)
  , TellOutput(..)
  , handleTell
  ) where

import qualified Data.Set                   as S

import           Reactive.Banana

import           Part4.Common.Util
import           Part4.Types
import           Part4.Types.NameError
import           Part4.Types.Notification
import           Part4.Types.PrivateMessage

data TellInput = TellInput {
    tibNames          :: Behavior (S.Set User)
  , tibName           :: Behavior User
  , tiePrivateMessage :: Event PrivateMessage
  }

data TellOutput = TellOutput {
    toeNotify :: Event Notification
  , toeError  :: Event String
  }

processTell :: S.Set User -> PrivateMessage -> Either String PrivateMessage
processTell names pm@(PrivateMessage to _) =
  case checkValidNameInUse names to of
    Left e ->
      Left $ nameErrorMessage e
    Right _ ->
      Right pm

checkNotSelf :: User -> PrivateMessage -> Either String PrivateMessage
checkNotSelf name pm@(PrivateMessage target _)
  | name == target = Left "Stop talking to yourself"
  | otherwise      = Right pm

handleTell :: MonadMoment m => TellInput -> m TellOutput
handleTell (TellInput bNames bName ePrivateMessage) = do
  let
    eSelfError = never
    eNotSelf = ePrivateMessage
    -- (eSelfError, eNotSelf) = split $ checkNotSelf <$> bName <@> ePrivateMessage
    (eNameError, eNameValid) = split $ processTell <$> bNames <@> eNotSelf
    eNotify = (\f (PrivateMessage t m) -> NTell f t m) <$> bName <@> eNameValid
    eError = leftmost [
        eSelfError
      , eNameError
      ]
  return $ TellOutput eNotify eError
