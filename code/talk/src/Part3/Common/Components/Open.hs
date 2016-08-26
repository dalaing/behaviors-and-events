{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Common.Components.Open (
    OpenInput(..)
  , OpenOutput(..)
  , handleOpen
  ) where

import Reactive.Banana

data OpenInput  = OpenInput  { oieOpen :: Event () }
data OpenOutput = OpenOutput { ooeWrite :: Event String }

handleOpen :: MonadMoment m => OpenInput -> m OpenOutput
handleOpen (OpenInput eOpen) =
  let
    eWrite = "Hi (type /help for instructions)" <$ eOpen
  in
    return $ OpenOutput eWrite
