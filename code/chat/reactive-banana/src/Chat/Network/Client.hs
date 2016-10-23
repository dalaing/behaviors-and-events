{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
module Chat.Network.Client (
    ClientInput(..)
  , ClientOutput(..)
  , clientNetwork
  ) where

import           Control.Monad                       (join)

import qualified Data.Map                            as M
import qualified Data.Set                            as S
import qualified Data.Text                           as T

import           Reactive.Banana                     (Behavior, Event, Moment,
                                                      filterE, never, stepper)

import           Chat.Components.Command             (CommandInput (..),
                                                      CommandOutput (..),
                                                      handleCommand)
import           Chat.Components.Name                (NameInput (..),
                                                      NameOutput (..))
import qualified Chat.Components.Name.Interactive    as NaI (handleName)
import qualified Chat.Components.Name.NonInteractive as NaNI (handleName)
import           Chat.Components.Notification        (NotifyInput (..),
                                                      NotifyOutput (..))
import qualified Chat.Components.Notification.Batch  as NoB (handleNotify)
import qualified Chat.Components.Notification.Stream as NoS (handleNotify)
import           Chat.Network.Types                  (LineInput (..),
                                                      LineOutput (..))
import           Chat.Types.Config                   (Config (..))
import           Chat.Types.Name                     (Name, NameType (..))
import           Chat.Types.Notification             (Notification (..),
                                                      NotificationType (..))
import           Util                                (leftmost)
import           Util.Switch                         (Switch (..), switchAp)

data WrappedOutput =
  WrappedOutput {
    woeName   :: Event Name
  , woeFetch  :: Event ()
  , woeKick   :: Event Int
  , woeNotify :: Event Notification
  , woeWrite  :: Event T.Text
  , woeClose  :: Event ()
  }

instance Switch WrappedOutput where
  switch e ee =
    WrappedOutput <$>
      switchAp woeName e ee <*>
      switchAp woeFetch e ee <*>
      switchAp woeKick e ee <*>
      switchAp woeNotify e ee <*>
      switchAp woeWrite e ee <*>
      switchAp woeClose e ee

wrapName :: NameOutput -> WrappedOutput
wrapName (NameOutput eName eNotify eWrite) =
  WrappedOutput eName never never eNotify eWrite never

wrapCommand :: CommandOutput -> WrappedOutput
wrapCommand (CommandOutput eFetch eKick eNotify eWrite eClose) =
  WrappedOutput never eFetch eKick eNotify eWrite eClose

data ClientInput =
  ClientInput {
    ciId         :: Int
  , cibLimit     :: Behavior Int
  , cibNameIdMap :: Behavior (M.Map Name Int)
  , cieNotifyIn  :: Event [Notification]
  , cieKickIn    :: Event (S.Set Int)
  , ciIO         :: LineInput
  }

data ClientOutput =
  ClientOutput {
    cobName      :: Behavior (Maybe Name)
  -- this can probably drop the Maybe
  , coeName      :: Event (Maybe Name)
  , coeNotifyOut :: Event Notification
  , coeKick      :: Event Int
  , coIO         :: LineOutput
  }

handleName :: Config -> NameInput -> Moment NameOutput
handleName config =
  case cNameType config of
    Interactive    -> NaI.handleName
    NonInteractive -> NaNI.handleName

handleNotify :: Config -> NotifyInput -> Moment NotifyOutput
handleNotify config =
  case cNotificationType config of
    Stream -> NoS.handleNotify
    Batch  -> NoB.handleNotify

-- Need to break this down so that we can use it for the HTTP backend
-- - eOpened and eClosed go through as before

-- - probably want an eRead -> Event ParsedCommands kind of thing
--   - one function for each phase, each passed into the read form of clientNetwork
-- - maybe just have two version of client network? parametise ClientInput on the per-client events it takes in?

-- ParsedCommands eName ..list of commands from command phase... eUnknown
-- - possibly fold opened and closed into this as well and have DomainInput or something like that
-- - should probably rename InputIO to IOInput or LineInput while we're at it
--
-- Eventually would be nice to use classy prisms to replace the text output with a data structure output,
-- that then gets converted to text
clientNetwork :: Config -> ClientInput -> Moment ClientOutput
clientNetwork config (ClientInput cId bLimit bNameIdMap eNotifyIn eKickIn (LineInput eOpened eRead eClosed)) = mdo
  let
    bNames = M.keysSet <$> bNameIdMap
    nameOut = fmap wrapName . handleName config $ NameInput bNames eOpened eRead
    commandOut = fmap wrapCommand . handleCommand config $ CommandInput cId bNameIdMap bName' eRead eClosed

  wo <- join $ switch nameOut (commandOut <$ eName')

  let
    eName' = woeName wo
  bName' <- stepper "" eName'
  let
    eName = fmap Just eName'
  bName <- stepper Nothing eName

  let
    eFetch = woeFetch wo
    eKickOut = woeKick wo
    eNotifyOut = woeNotify wo

  -- we kind of want to switchPromptly on eName
  eNotifyIn' <- switch ((pure . NJoin) <$> eName') . leftmost $ [
      eNotifyIn <$ eName
    , never <$ eClose
    ]
  NotifyOutput enWrite <- handleNotify config $ NotifyInput bLimit eFetch eNotifyIn'

  let
    eWrite = leftmost [woeWrite wo, enWrite]
    eClose = leftmost [woeClose wo, () <$ filterE (S.member cId) eKickIn]

  return $ ClientOutput bName eName eNotifyOut eKickOut (LineOutput eWrite eClose)
