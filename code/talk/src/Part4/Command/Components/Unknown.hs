{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command.Components.Unknown (
    UnknownInput(..)
  , UnknownOutput(..)
  , handleUnknown
  ) where

import Reactive.Banana

data UnknownInput = UnknownInput {
    uieUnknown :: Event String
  }

data UnknownOutput = UnknownOutput {
    uieWrite :: Event String
  }

unknownMessage :: String -> String
unknownMessage "" =
  "Command can not be an empty string.\nType /help for options."
unknownMessage cmd =
  "Unknown command: " ++ cmd ++ ".\nType /help for options."

handleUnknown :: MonadMoment m => UnknownInput -> m UnknownOutput
handleUnknown (UnknownInput eUnknown) = do
  let
    eWrite = unknownMessage <$> eUnknown
  return $ UnknownOutput eWrite
