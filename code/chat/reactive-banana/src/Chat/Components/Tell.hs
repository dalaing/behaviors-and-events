{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Components.Tell (
    TellInput(..)
  , TellOutput(..)
  , keywordTell
  , parseTell
  , helplineTell
  , handleTell
  ) where

import           Control.Monad           (guard)
import           Data.Bifunctor          (first)

import qualified Data.Set                as S
import qualified Data.Text               as T

import           Reactive.Banana         (Behavior, Event, MonadMoment, split,
                                          (<@>))

import           Chat.Types.Message      (PrivateMessage (..))
import           Chat.Types.Name         (Name, checkNameAlreadyInUse,
                                          nameErrorText)
import           Chat.Types.Notification (Notification (..))

data TellInput = TellInput {
    tibNames :: Behavior (S.Set Name)
  , tibName  :: Behavior Name
  , tieTell  :: Event PrivateMessage
  }

data TellOutput = TellOutput {
    toeNotify :: Event Notification
  , toeWrite  :: Event T.Text
  }

keywordTell :: T.Text
keywordTell =
  "tell"

parseTell :: T.Text
          -> Maybe PrivateMessage
parseTell cmd = do
  let
    (kw, rest1) = T.splitAt (T.length keywordTell) cmd
  guard $ kw == keywordTell
  (h, t) <- T.uncons rest1
  guard $ h == ' '
  let
    (name, rest2) = T.break (== ' ') t
    msg = T.tail rest2
  return $ PrivateMessage name msg

helplineTell :: T.Text
helplineTell =
  "/tell <user> <msg> - sends a private message to a user"

checkTellName :: S.Set Name
              -> Name
              -> PrivateMessage
              -> Either T.Text PrivateMessage
checkTellName names messager (PrivateMessage messagee m) = do
  n' <- first nameErrorText $ checkNameAlreadyInUse names messagee
  if messager == messagee
  then Left "Stop talking to yourself"
  else Right $ PrivateMessage n' m

handleTell :: MonadMoment m
           => TellInput
           -> m TellOutput
handleTell (TellInput bNames bName eTell) =
  let
    (eFeedback, eTellValid) = split $ checkTellName <$> bNames <*> bName <@> eTell
    eNotify = (\f (PrivateMessage t m) -> NTell f t m) <$> bName <@> eTellValid
  in
    return $ TellOutput eNotify eFeedback
