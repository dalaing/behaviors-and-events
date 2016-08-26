{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Common.Components.Unknown (
    UnknownInput(..)
  , UnknownOutput(..)
  , handleUnknown
  ) where

import Reactive.Banana

data UnknownInput  = UnknownInput  { ucieCommand :: Event String }
data UnknownOutput = UnknownOutput { ucoeWrite :: Event String }

handleUnknown :: MonadMoment m => UnknownInput -> m UnknownOutput
handleUnknown (UnknownInput eUnknown) =
  let
      msg x = "Unknown command: " ++ x ++ " (type /help for instructions)"
  in
    return . UnknownOutput $ msg <$> eUnknown
