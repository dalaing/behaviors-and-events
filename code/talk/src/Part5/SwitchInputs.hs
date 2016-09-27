{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module Part5.SwitchInputs (
    networkDescription
  , go_5_si
  ) where

import qualified Data.Set as S

import Reactive.Banana
import Reactive.Banana.Frameworks

import Part4.Name
import Part4.Command
import Part4.Types.Notification
import Part4.Common.Util
import Part4.Common.IO

import Part5.Switch

instance Switch InputIO where
  switch e ee =
    InputIO <$>
      switchAp ioeOpen e ee <*>
      switchAp ioeRead e ee

instance Switch OutputIO where
  switch e ee =
    OutputIO <$>
      switchAp ioeWrite e ee <*>
      switchAp ioeClose e ee

instance Switch CommandOutput where
  switch e ee =
    CommandOutput <$>
      switchAp coeWrite e ee <*>
      switchAp coeClose e ee <*>
      switchAp coeNotify e ee <*>
      switchAp coeFetch e ee <*>
      switchAp coeKick e ee

data Phase =
    PreOpen
  | NamePrompting
  | CommandProcessing
  deriving (Eq, Ord, Show)

networkDescription :: InputIO -> MomentIO OutputIO
networkDescription (InputIO eOpen eRead) = mdo
  let
    bGreeting = pure "Welcome to the chat server."
  bNames <- accumB (S.fromList ["root", "admin"]) (S.insert <$> eName)

  eName <- once enName

  let
    enRead = whenE ((== NamePrompting) <$> bPhase) eRead
  NameOutput enWrite enNotify enName <- liftMoment . handleName $ NameInput eOpen enRead bGreeting bNames

  bPhase <- stepper PreOpen . leftmost $ [
      NamePrompting <$ eOpen
    , CommandProcessing <$ eName
    ]
  bName <- stepper "" eName

  let
    ecRead = whenE ((== CommandProcessing) <$> bPhase) eRead
  CommandOutput ecWrite ecClose ecNotify  eFetch _ <- liftMoment . handleCommand $ CommandInput ecRead bNames bName

  let
    eNotify = leftmost [enNotify, ecNotify]
  NotificationOutput enoWrite <- handleNotification Stream $ NotificationInput bName eFetch eNotify

  let
    nameOut = OutputIO (leftmost [enWrite, enoWrite]) never
    cmdOut = OutputIO (leftmost [ecWrite, enoWrite]) ecClose

  switch nameOut (cmdOut <$ eName)

go_5_si :: IO ()
go_5_si = mkGo . mkNetwork $ networkDescription
