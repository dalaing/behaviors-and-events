{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part5.Names.Example1.Cmd (
    CmdInput(..)
  , CmdOutput(..)
  , handleCmd
  , handleCmd2
  ) where

import qualified Data.Set        as S

import           Reactive.Banana

import Part5.Names.Example1.Common

data CmdInput = CmdInput {
    cieStart :: Event ()
  , cieRead  :: Event String
  , cibNames :: Behavior (S.Set String)
  , cibName  :: Behavior String
  }

data CmdOutput = CmdOutput {
    coeWrite :: Event String
  , coeStop  :: Event ()
  }

handleCmd :: MonadMoment m => CmdInput -> m CmdOutput
handleCmd (CmdInput eStart eRead bNames bName) = do
  let
    eWrite = leftmost ["boom" <$ eStart, "bam" <$ eRead]
  return $ CmdOutput eWrite never

handleCmd2 :: MonadMoment m => CmdInput -> (Event String, m CmdOutput)
handleCmd2 (CmdInput eStart eRead bNames bName) =
  let
    open =
      "boom" <$ eStart
    network =
      return $ CmdOutput ("bam" <$ eRead) never
  in
    (open, network)
