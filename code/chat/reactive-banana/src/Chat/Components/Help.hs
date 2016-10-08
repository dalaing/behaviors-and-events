{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Components.Help (
    HelpInput(..)
  , HelpOutput(..)
  , keywordHelp
  , handleHelp
  , parseHelp
  ) where

import           Data.Maybe                   (fromMaybe)

import qualified Data.Text                    as T

import           Reactive.Banana              (Event, MonadMoment)

import           Chat.Components.Kick         (helplineKick)
import           Chat.Components.Message      (helplineMessage)
import           Chat.Components.Notification (helplineFetch)
import           Chat.Components.Quit         (helplineQuit)
import           Chat.Components.Tell         (helplineTell)
import           Chat.Types.Config            (Config)

data HelpInput = HelpInput {
    hieHelp :: Event ()
  }

data HelpOutput = HelpOutput {
    hoeWrite :: Event T.Text
  }

helpMessage :: Config
            -> T.Text
helpMessage c =
  T.intercalate "\n" $
    [ helplineMessage
    , helplineTell
    , helplineKick
    ] ++
    fromMaybe [] (pure <$> helplineFetch c) ++
    [helplineHelp
    , helplineQuit
    ]

keywordHelp :: T.Text
keywordHelp =
  "help"

parseHelp :: T.Text
          -> Maybe ()
parseHelp cmd
 | cmd == keywordHelp = Just ()
 | otherwise          = Nothing

helplineHelp :: T.Text
helplineHelp =
  "/help              - displays this message"

handleHelp :: MonadMoment m
           => Config
           -> HelpInput
           -> m HelpOutput
handleHelp config (HelpInput eHelp) =
  let
    eWrite = helpMessage config <$ eHelp
  in
    return $ HelpOutput eWrite
