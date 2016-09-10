{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module Part5.SwitchBlock (
    networkDescription
  , go_5_sb
  ) where

import Control.Monad

import qualified Data.Set                   as S

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Part4.Command
import           Part4.Common.IO
import           Part4.Common.Util
import           Part4.Name
import           Part4.Types.Notification

import           Part5.Switch

data OutputWrapper = OutputWrapper {
    owWrite  :: Event String
  , owClose  :: Event ()
  , owName   :: Event String
  , owNotify :: Event Notification
  , owKick   :: Event String
  }

instance Switch OutputWrapper where
  switch e ee =
    OutputWrapper <$>
      switchAp owWrite e ee <*>
      switchAp owClose e ee <*>
      switchAp owName e ee <*>
      switchAp owNotify e ee <*>
      switchAp owKick e ee

wrapName :: NameOutput -> OutputWrapper
wrapName (NameOutput eWrite eName) =
  OutputWrapper eWrite never eName never never

wrapCmd :: CommandOutput -> OutputWrapper
wrapCmd (CommandOutput eWrite eClose eNotify eKick) =
  OutputWrapper eWrite eClose never eNotify eKick

networkDescription :: InputIO -> Moment OutputIO
networkDescription (InputIO eOpen eRead) = mdo
  let
    bGreeting = pure "Welcome to the chat server."
  bNames <- accumB (S.fromList ["root", "admin"]) (S.insert <$> eName)

  -- the recursive use of eNotify is not working
  -- we're also doing observe-then-switch
  -- would be nice to switch-then-observe

  let
    nameBlock = fmap wrapName . handleName $ NameInput eOpen eRead bGreeting bNames
    cmdBlock = fmap wrapCmd . handleCommand Stream $ CommandInput eName eRead eNotify bNames bName

  -- switch and then observeE means we need a switch returning (Event (Moment a)), from which we'll get a
  -- Moment a
  -- observeE and then switch is easier to deal with

  OutputWrapper eWrite eClose eName eNotify eKick <- join $ switch nameBlock (cmdBlock <$ eName)

  bName <- stepper "" eName

  -- this has a problem with eName coming through because it is used with the switch..
  return $ OutputIO eWrite eClose


go_5_sb :: IO ()
go_5_sb = mkGo . mkNetwork $ networkDescription
