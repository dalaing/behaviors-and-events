{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Chat.Components.Command (
    CommandInput(..)
  , CommandOutput(..)
  , handleCommand
  ) where

import qualified Data.Set                            as S
import qualified Data.Text                           as T

import           Reactive.Banana                     (Behavior, Event,
                                                      MonadMoment, filterJust,
                                                      split)

import           Chat.Types.Config                   (Config (..))
import           Chat.Types.Message                  (Message, PrivateMessage)
import           Chat.Types.Name                     (Name)
import           Chat.Types.Notification             (Notification)

import           Chat.Components.Disconnected        (DisconnectedInput (..),
                                                      DisconnectedOutput (..),
                                                      handleDisconnected)
import           Chat.Components.Help                (HelpInput (..),
                                                      HelpOutput (..),
                                                      handleHelp, parseHelp)
import           Chat.Components.Kick                (KickInput (..),
                                                      KickOutput (..),
                                                      handleKick, parseKick)
import           Chat.Components.Message             (MessageInput (..),
                                                      MessageOutput (..),
                                                      handleMessage)
import           Chat.Components.Notification        (parseFetch)
import           Chat.Components.Quit                (QuitInput (..),
                                                      QuitOutput (..),
                                                      handleQuit, parseQuit)
import           Chat.Components.Tell                (TellInput (..),
                                                      TellOutput (..),
                                                      handleTell, parseTell)
import           Chat.Components.Unknown             (UnknownInput (..),
                                                      UnknownOutput (..),
                                                      handleUnknown,
                                                      parseUnknown)
import           Util                                (leftmost)

data CommandInput = CommandInput {
    cibNames     :: Behavior (S.Set Name)
  , cibName      :: Behavior Name
  , cieRead      :: Event T.Text
  , cieHasClosed :: Event ()
  }

data FanCommand = FanCommand {
    fceMessage :: Event Message
  , fceTell    :: Event PrivateMessage
  , fceKick    :: Event Name
  , fceFetch   :: Event ()
  , fceHelp    :: Event ()
  , fceQuit    :: Event ()
  , fceUnknown :: Event T.Text
  }

data CommandOutput = CommandOutput {
    coeKick   :: Event Name
  , coeFetch  :: Event ()
  , coeNotify :: Event Notification
  , coeWrite  :: Event T.Text
  , coeClose  :: Event ()
  }

splitCommand :: T.Text
             -> Either T.Text T.Text
splitCommand t
  | (not . T.null $ t) && T.head t == '/' =
    Right $ T.tail t
  | otherwise =
    Left t

fanCommand :: MonadMoment m
              => Config
              -> Event T.Text
              -> m FanCommand
fanCommand config eRead =
  let
    (eMessage, eCommand) =
      split $ splitCommand <$> eRead

    eTell =
      filterJust $ parseTell <$> eCommand
    eKick =
      filterJust $ parseKick <$> eCommand
    eFetch =
      filterJust $ parseFetch config <$> eCommand
    eHelp =
      filterJust $ parseHelp <$> eCommand
    eQuit =
      filterJust $ parseQuit <$> eCommand
    eUnknown =
      filterJust $ parseUnknown config <$> eCommand
  in
    return $ FanCommand eMessage eTell eKick eFetch eHelp eQuit eUnknown

handleCommand :: MonadMoment m
              => Config
              -> CommandInput
              -> m CommandOutput
handleCommand config (CommandInput bNames bName eRead eClosed) = do
  FanCommand eMessage eTell eKick eFetch eHelp eQuit eUnknown <- fanCommand config eRead
  MessageOutput emNotify <- handleMessage $ MessageInput bName eMessage
  TellOutput etNotify etWrite <- handleTell $ TellInput bNames bName eTell
  KickOutput eKickValid ekNotify ekWrite <- handleKick $ KickInput bNames bName eKick
  HelpOutput ehWrite <- handleHelp config $ HelpInput eHelp
  QuitOutput eqNotify eqClose <- handleQuit $ QuitInput bName eQuit
  DisconnectedOutput edNotify edClose <- handleDisconnected $ DisconnectedInput bName eClosed
  UnknownOutput euWrite <- handleUnknown $ UnknownInput eUnknown
  let
    eNotify =
      leftmost [
        emNotify
      , etNotify
      , ekNotify
      , eqNotify
      , edNotify
      ]
    eWrite =
      leftmost [
        etWrite
      , ekWrite
      , ehWrite
      , euWrite
      ]
    eClose =
      leftmost [
        eqClose
      , edClose
      ]
  return $ CommandOutput eKickValid eFetch eNotify eWrite eClose
