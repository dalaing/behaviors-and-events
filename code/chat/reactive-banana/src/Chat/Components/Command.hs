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

import qualified Data.Text                    as T

import           Reactive.Banana              (Event, MonadMoment, filterJust,
                                               split)

import           Chat.Types.Config            (Config (..))
import           Chat.Types.Message           (Message, PrivateMessage)
import           Chat.Types.Name              (Name)

import           Chat.Components.Help         (parseHelp)
import           Chat.Components.Kick         (parseKick)
import           Chat.Components.Notification (parseFetch)
import           Chat.Components.Quit         (parseQuit)
import           Chat.Components.Tell         (parseTell)
import           Chat.Components.Unknown      (parseUnknown)

data CommandInput = CommandInput {
    cieRead :: Event T.Text
  }

data CommandOutput = CommandOutput {
    coeMessage :: Event Message
  , coeTell    :: Event PrivateMessage
  , coeKick    :: Event Name
  , coeFetch   :: Event ()
  , coeHelp    :: Event ()
  , coeQuit    :: Event ()
  , coeUnknown :: Event T.Text
  }

splitCommand :: T.Text
             -> Either T.Text T.Text
splitCommand t
  | T.head t == '/' =
    Right $ T.tail t
  | otherwise =
    Left t

handleCommand :: MonadMoment m
              => Config
              -> CommandInput
              -> m CommandOutput
handleCommand config (CommandInput eRead) =
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
    return $ CommandOutput eMessage eTell eKick eFetch eHelp eQuit eUnknown
