{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
module Local.Network (
    network
  ) where

import           Control.Monad                       (join)
import           Data.IORef                          (writeIORef)

import           Data.Set                            as S
import           Data.Text                           as T

import           Reactive.Banana                     (Event, Moment,
                                                      MonadMoment (..), accumB,
                                                      never, stepper)
import           Reactive.Banana.Frameworks          (MomentIO, fromAddHandler,
                                                      reactimate)

import           Chat.Components.Command             (CommandInput (..),
                                                      CommandOutput (..),
                                                      handleCommand)
import           Chat.Components.Name                (NameInput (..),
                                                      NameOutput (..))
import           Chat.Components.Name.Interactive    (handleName)
import           Chat.Components.Notification        (NotifyInput (..),
                                                      NotifyOutput (..))
import           Chat.Components.Notification.Stream (handleNotify)
import           Chat.Types.Config                   (Config (..))
import           Chat.Types.Name                     (Name, NameType (..))
import           Chat.Types.Notification             (Notification,
                                                      NotificationType (..))
import           Local.InputSources                  (InputSources (..))
import           Util                                (leftmost)
import           Util.IO                             (EventSource (..))
import           Util.Switch                         (Switch (..), switchAp)

data InputIO = InputIO {
    ieOpen  :: Event ()
  , ieRead  :: Event T.Text
  , ieClose :: Event ()
  }

handleInput :: InputSources -> MomentIO InputIO
handleInput (InputSources esOpen esRead esClose _) = do
  eOpen  <- fromAddHandler . addHandler $ esOpen
  eRead  <- fromAddHandler . addHandler $ esRead
  eClose <- fromAddHandler . addHandler $ esClose
  return $ InputIO eOpen eRead eClose

data OutputIO = OutputIO {
    oeWrite :: Event T.Text
  , oeClose :: Event ()
  }

handleOutput :: InputSources -> OutputIO -> MomentIO ()
handleOutput (InputSources _ _ _ refClose) (OutputIO eWrite eClose) = do
  reactimate $ putStrLn . T.unpack <$> eWrite
  reactimate $ writeIORef refClose True <$ eClose

network :: InputSources -> MomentIO ()
network is = do
  i <- handleInput is
  o <- liftMoment $ network' i
  handleOutput is o

data WrappedOutput =
  WrappedOutput {
    woeName   :: Event Name
  , woeKick   :: Event Name
  , woeFetch  :: Event ()
  , woeNotify :: Event Notification
  , woeWrite  :: Event T.Text
  , woeClose  :: Event ()
  }

instance Switch WrappedOutput where
  switch e ee =
    WrappedOutput <$>
      switchAp woeName e ee <*>
      switchAp woeKick e ee <*>
      switchAp woeFetch e ee <*>
      switchAp woeNotify e ee <*>
      switchAp woeWrite e ee <*>
      switchAp woeClose e ee

wrapName :: NameOutput -> WrappedOutput
wrapName (NameOutput eName eNotify eWrite) =
  WrappedOutput eName never never eNotify eWrite never

wrapCommand :: CommandOutput -> WrappedOutput
wrapCommand (CommandOutput eKick eFetch eNotify eWrite eClose) =
  WrappedOutput never eKick eFetch eNotify eWrite eClose

network' :: InputIO -> Moment OutputIO
network' (InputIO eOpened eRead eClosed) = mdo
  let
    config = Config Interactive Stream

    nameOut = fmap wrapName . handleName $ NameInput bNames eOpened eRead
    commandOut = fmap wrapCommand . handleCommand config $ CommandInput bNames bName eRead eClosed

  wo <- join $ switch nameOut (commandOut <$ eName)

  let
    eName = woeName wo
  bName <- stepper "" eName
  bNames <- accumB (S.fromList ["admin"]) $
    S.insert <$> eName

  let
    bLimit = pure 100
    eFetch = woeFetch wo
    eNotify = woeNotify wo

  NotifyOutput enWrite <- handleNotify $ NotifyInput bLimit eFetch eNotify

  let
    eWrite = leftmost [woeWrite wo, enWrite]
    eClose = woeClose wo

  return $ OutputIO eWrite eClose
