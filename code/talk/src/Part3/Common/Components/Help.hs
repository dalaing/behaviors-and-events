{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Common.Components.Help (
    HelpInput(..)
  , HelpOutput(..)
  , handleHelp
  ) where

import Reactive.Banana

data HelpInput  = HelpInput  { hieHelp :: Event () }
data HelpOutput = HelpOutput { hoeWrite :: Event String }

-- TODO would be nice if the other components could contribute their own help strings
handleHelp :: MonadMoment m => HelpInput -> m HelpOutput
handleHelp (HelpInput eHelp) =
  let
    helpStrings = [
        "/limitup increases the history limit"
      , "/limitdown decreases the history limit"
      , "/help displays this message"
      , "/quit exits the program"
      ]
    eWrite = unlines helpStrings <$ eHelp
  in
    return $ HelpOutput eWrite
