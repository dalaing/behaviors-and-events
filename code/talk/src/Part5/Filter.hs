{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module Part5.Filter (
    networkDescription
  , go_5_f
  ) where

import qualified Data.Set as S

import Reactive.Banana

import Part4.Name
import Part4.Command
import Part4.Types.Notification
import Part4.Common.Util
import Part4.Common.IO

data Phase =
    PreOpen
  | NamePrompting
  | CommandProcessing
  deriving (Eq, Ord, Show)

networkDescription :: InputIO -> Moment OutputIO
networkDescription (InputIO eOpen eRead) = mdo
  let
    bGreeting = pure "Welcome to the chat server."
    -- bNames = pure . S.fromList $ ["root", "admin"]
  bNames <- accumB (S.fromList ["root", "admin"]) (S.insert <$> eName)

  let
    enRead = whenE ((== NamePrompting) <$> bPhase) eRead
  NameOutput enWrite enNotify eName <- handleName $ NameInput eOpen enRead bGreeting bNames

  bPhase <- stepper PreOpen . leftmost $ [
      NamePrompting <$ eOpen
    , CommandProcessing <$ eName
    ]
  bName <- stepper "" eName

  -- what do all of these look like when you change the nickname?
  let
    ecRead = whenE ((== CommandProcessing) <$> bPhase) eRead
  CommandOutput ecWrite eClose ecNotify eFetch _ <- handleCommand $ CommandInput ecRead bNames bName

  let
    eNotify = leftmost [enNotify, ecNotify]
  NotificationOutput enoWrite <- handleNotification Stream $ NotificationInput bName eFetch eNotify

  let
    eWrite = leftmost [enWrite, ecWrite, enoWrite]
  return $ OutputIO eWrite eClose

go_5_f :: IO ()
go_5_f = mkGo . mkNetwork $ networkDescription
