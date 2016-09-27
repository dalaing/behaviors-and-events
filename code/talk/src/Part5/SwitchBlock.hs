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

import           Control.Monad

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
  , owFetch  :: Event ()
  , owKick   :: Event String
  }

instance Switch OutputWrapper where
  switch e ee =
    OutputWrapper <$>
      switchAp owWrite e ee <*>
      switchAp owClose e ee <*>
      switchAp owName e ee <*>
      switchAp owNotify e ee <*>
      switchAp owFetch e ee <*>
      switchAp owKick e ee

wrapName :: NameOutput -> OutputWrapper
wrapName (NameOutput eWrite eNotify eName) =
  OutputWrapper eWrite never eName eNotify never never

wrapCmd :: CommandOutput -> OutputWrapper
wrapCmd (CommandOutput eWrite eClose eNotify eFetch eKick) =
  OutputWrapper eWrite eClose never eNotify eFetch eKick

networkDescription :: InputIO -> Moment OutputIO
networkDescription (InputIO eOpen eRead) = mdo
  let
    bGreeting = pure "Welcome to the chat server."
  bNames <- accumB (S.fromList ["root", "admin"]) (S.insert <$> eName)

  eName <- once eowName

  let
    nameBlock = fmap wrapName . handleName $ NameInput eOpen eRead bGreeting bNames
    cmdBlock = fmap wrapCmd . handleCommand $ CommandInput eRead bNames bName

  OutputWrapper eowWrite eClose eowName eNotify eFetch eKick <- join $ switch nameBlock (cmdBlock <$ eName)

  bName <- stepper "" eName

  NotificationOutput enoWrite <- handleNotification Stream $ NotificationInput bName eFetch eNotify

  let
    eWrite = leftmost [eowWrite, enoWrite]

  return $ OutputIO eWrite eClose


go_5_sb :: IO ()
go_5_sb = mkGo . mkNetwork $ networkDescription
