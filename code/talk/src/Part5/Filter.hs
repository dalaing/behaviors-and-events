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

  NameOutput enWrite eName <- handleName $ NameInput eOpen enRead bGreeting bNames

  bPhase <- stepper PreOpen . leftmost $ [
      NamePrompting <$ eOpen
    , CommandProcessing <$ eName
    ]
  bName <- stepper "" eName

  -- if we use () <$ eName to trigger the open, we'll end up sampling bName before the name has come through when
  -- we try to create the open notification
  -- alternative: add a delay (moves things into IO)
  -- alternative: have handleCommand return (a, m b), fmap eName over that 
  -- - observe Event (m b) to make it happen
  -- - use Event a as output - how does it get into the notification stream?
  -- alternative: handleCommand returns (a -> m b), fmap over eName :: Event a and observe
  -- - can use that to set up a pure behavior for the name
  -- what do all of these look like when you change the nickname?
  CommandOutput ecWrite eClose ecNotify _ <- handleCommand Stream $ CommandInput eName ecRead ecNotify bNames bName

  let
    enRead = whenE ((== NamePrompting) <$> bPhase) eRead
    ecRead = whenE ((== CommandProcessing) <$> bPhase) eRead

  let
    eWrite = leftmost [enWrite, ecWrite]
  return $ OutputIO eWrite eClose

go_5_f :: IO ()
go_5_f = mkGo . mkNetwork $ networkDescription
