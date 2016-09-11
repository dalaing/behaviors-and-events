{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Name (
    NameInput(..)
  , NameOutput(..)
  , handleName
  ) where

import           Data.List                     (intercalate)

import qualified Data.Set                      as S

import           Reactive.Banana

import           Part4.Command.Components.Open
import           Part4.Common
import           Part4.Types.NameError
import           Part4.Types.Notification

data NameInput = NameInput {
    nieOpen     :: Event ()
  , nieRead     :: Event String
  , nibGreeting :: Behavior String
  , nibNames    :: Behavior (S.Set String)
  }

data NameOutput = NameOutput {
    noeWrite        :: Event String
  , noeNotification :: Event Notification
  , noeName         :: Event String
  }

mkGreeting :: String -> String -> String
mkGreeting greeting prompt =
  intercalate "\n" [greeting, prompt]

mkNameErrorMessage :: String -> NameError -> String
mkNameErrorMessage prompt e =
  intercalate "\n" [nameErrorMessage e, prompt]

handleName :: NameInput -> Moment NameOutput
handleName (NameInput eOpen eRead bGreeting bNames) = do
  let
    bPrompt = pure "Please enter your name:"

    eGreeting = mkGreeting <$> bGreeting <*> bPrompt <@ eOpen

    (eNameInvalid, eNameValid) = split $ checkValidNameNotInUse <$> bNames <@> eRead

    eNameErrorMessage = mkNameErrorMessage <$> bPrompt <@> eNameInvalid

    eWrite = leftmost [
        eGreeting
      , eNameErrorMessage
      ]

  OpenOutput eNotify <- handleOpen $ OpenInput eNameValid

  return $ NameOutput eWrite eNotify eNameValid
