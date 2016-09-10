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

networkDescription :: InputIO -> Moment OutputIO
networkDescription (InputIO eOpen eRead) = mdo
  let
    bGreeting = pure "Welcome to the chat server."
  bNames <- accumB (S.fromList ["root", "admin"]) (S.insert <$> eName)

  let
    emptyInput = InputIO never never
    regularInput = InputIO eOpen eRead

  (InputIO enOpen enRead) <- switch regularInput (emptyInput <$ eName)
  (InputIO _ ecRead)      <- switch emptyInput   (regularInput <$ eName)

  NameOutput enWrite eName <- handleName $ NameInput enOpen enRead bGreeting bNames

  bName <- stepper "" eName

  -- this has a problem with eName coming through because it is used with the switch..
  CommandOutput ecWrite ecClose ecNotify _ <- handleCommand Stream $ CommandInput eName ecRead ecNotify bNames bName

  let
    nameOut = OutputIO enWrite never
    cmdOut = OutputIO ecWrite ecClose

  switch nameOut (cmdOut <$ eName)

go_5_si :: IO ()
go_5_si = mkGo . mkNetwork $ networkDescription
