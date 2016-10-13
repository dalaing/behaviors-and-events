{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Components.Kick (
    KickInput(..)
  , KickOutput(..)
  , keywordKick
  , parseKick
  , helplineKick
  , handleKick
  ) where

import           Control.Monad           (guard)
import           Data.Bifunctor          (first)

import qualified Data.Set                as S
import qualified Data.Text               as T

import           Reactive.Banana         (Behavior, Event, MonadMoment, split,
                                          (<@>))

import           Chat.Types.Name         (Name, checkNameAlreadyInUse,
                                          nameErrorText)
import           Chat.Types.Notification (Notification (..))

data KickInput = KickInput {
    kibNames :: Behavior (S.Set Name)
  , kibName  :: Behavior Name
  , kieKick  :: Event Name
  }

data KickOutput = KickOutput {
    koeKickValid :: Event Name
  , koeNotify    :: Event Notification
  , koeFeedback  :: Event T.Text
  }

keywordKick :: T.Text
keywordKick =
  "kick"

parseKick :: T.Text
          -> Maybe Name
parseKick cmd = do
  let
    (kw, rest) = T.splitAt (T.length keywordKick) cmd
  guard $ kw == keywordKick
  (h, t) <- T.uncons rest
  guard $ h == ' ' && (not . T.null $ t)
  return t

helplineKick :: T.Text
helplineKick =
  "/kick <user>       - kicks a user"

checkKickName :: S.Set Name
              -> Name
              -> Name
              -> Either T.Text Name
checkKickName names kicker kickee = do
  n' <- first nameErrorText $ checkNameAlreadyInUse names kickee
  if kicker == kickee
  then Left "Stop kicking yourself"
  else Right n'

handleKick :: MonadMoment m
           => KickInput
           -> m KickOutput
handleKick (KickInput bNames bName eKick) =
  let
    (eFeedback, eKickValid) = split $ checkKickName <$> bNames <*> bName <@> eKick
    eNotify = NKick <$> bName <@> eKickValid
  in
    return $ KickOutput eKickValid eNotify eFeedback
