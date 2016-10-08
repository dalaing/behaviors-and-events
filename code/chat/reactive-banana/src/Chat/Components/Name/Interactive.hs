{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Components.Name.Interactive (
    handleName
  ) where

import qualified Data.Text                as T

import           Reactive.Banana          (MonadMoment, split, (<@>))

import           Chat.Components.Name                (NameInput (..), NameOutput (..))
import           Chat.Types.Name         (NameError, checkNameNotInUse,
                                           nameErrorText)
import           Chat.Types.Notification (Notification (..))
import           Util                     (leftmost)


mkGreeting :: T.Text
           -> T.Text
           -> T.Text
mkGreeting greeting prompt =
  T.intercalate "\n" [greeting, prompt]

mkNameErrorText :: T.Text
                -> NameError
                -> T.Text
mkNameErrorText prompt e =
  T.intercalate "\n" [nameErrorText e, prompt]

handleName :: MonadMoment m
           => NameInput
           -> m NameOutput
handleName (NameInput bNames eOpen eRead) = do

  let
    greeting = "Welcome to reactive-banana chat server!"
    prompt = "Please enter your name:"

    eGreeting = mkGreeting greeting prompt <$ eOpen

    (eNameInvalid, eNameValid) = split $ checkNameNotInUse <$> bNames <@> eRead
    eNotify = NJoin <$> eNameValid

    eErrorAndRetry = mkNameErrorText prompt <$> eNameInvalid

    eWrite = leftmost [
        eGreeting
      , eErrorAndRetry
      ]

  return $ NameOutput eNameValid eNotify eWrite
