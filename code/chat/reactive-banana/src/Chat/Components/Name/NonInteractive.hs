{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Components.Name.NonInteractive (
    handleName
  ) where

import           Reactive.Banana         (Moment, split, (<@>))

import           Chat.Components.Name    (NameInput (..), NameOutput (..))
import           Chat.Types.Name         (checkNameNotInUse, nameErrorText)
import           Chat.Types.Notification (Notification (..))

handleName :: NameInput
           -> Moment NameOutput
handleName (NameInput bNames _ eRead) = do

  let
    (eNameInvalid, eNameValid) = split $ checkNameNotInUse <$> bNames <@> eRead
    eNotify = NJoin <$> eNameValid
    eError = nameErrorText <$> eNameInvalid

  return $ NameOutput eNameValid eNotify eError
