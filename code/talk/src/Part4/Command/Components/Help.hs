{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command.Components.Help (
    HelpInput(..)
  , HelpOutput(..)
  , handleHelp
  ) where

import Data.List (intercalate)

import Reactive.Banana

data HelpInput = HelpInput {
    hieHelp :: Event ()
  }

data HelpOutput = HelpOutput {
    hieWrite :: Event String
  }

helpMessage :: String
helpMessage =
  intercalate "\n" [
      "<msg>              - sends a message to all users"
    , "/tell <user> <msg> - sends a private message to a user"
    , "/kick <user>       - kicks a user"
    , "/help              - displays this message"
    , "/quit              - exits the program"
    ]

handleHelp :: MonadMoment m => HelpInput -> m HelpOutput
handleHelp (HelpInput eHelp) = do
  let
    eWrite = helpMessage <$ eHelp
  return $ HelpOutput eWrite
