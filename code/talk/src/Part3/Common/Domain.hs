{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Part3.Common.Domain (
    Inputs(..)
  , Outputs(..)
  , liftDomainNetwork
  , InputsCmd(..)
  , OutputsCmd(..)
  ) where

import Control.Lens

import Reactive.Banana

import Part3.Common.Util
import Part3.Common.IO
import Part3.Common.Testing

data Inputs = Inputs {
    ieOpen             :: Event ()
  , ieMessage          :: Event String
  , ieHistoryLimitUp   :: Event ()
  , ieHistoryLimitDown :: Event ()
  , ieHelp             :: Event ()
  , ieQuit             :: Event ()
  , ieUnknownCommand   :: Event String
  }

data Outputs = Outputs {
    oeWrite :: [Event String]
  , oeClose :: [Event ()]
  }

fanOut :: InputIO -> Inputs
fanOut (InputIO eOpen eRead) =
  let
    eReadNonEmpty =
      filterE (not . null) eRead

    isMessage =
      (/= "/") . take 1
    eMessage =
      filterE isMessage eReadNonEmpty

    isCommand =
      (== "/") . take 1
    eCommand =
      fmap (drop 1) . filterE isCommand $ eReadNonEmpty

    eLimitUp   = () <$ filterE (== "limitup") eCommand
    eLimitDown = () <$ filterE (== "limitdown") eCommand
    eHelp      = () <$ filterE (== "help") eCommand
    eQuit      = () <$ filterE (== "quit") eCommand

    commands =
      ["limitup", "limitdown", "help", "quit"]
    eUnknownCommand =
      filterE (`notElem` commands) eCommand
  in
    Inputs eOpen eMessage eLimitUp eLimitDown eHelp eQuit eUnknownCommand

fanIn :: Outputs -> OutputIO
fanIn (Outputs eWrites eCloses) =
  let
    addLine x y = x ++ '\n' : y
    eCombinedWrites = foldr (unionWith addLine) never eWrites
    eCombinedCloses = () <$ leftmost eCloses
  in
    OutputIO eCombinedWrites eCombinedCloses

liftDomainNetwork :: MonadMoment m => (Inputs -> m Outputs) -> InputIO -> m OutputIO
liftDomainNetwork n i = do
  o <- n . fanOut $ i
  return $ fanIn o

data InputsCmd =
    Open
  | Message String
  | LimitUp
  | LimitDown
  | Help
  | Quit
  | Unknown String
  deriving (Eq, Ord, Show)

makePrisms ''InputsCmd

instance Fannable () InputsCmd where
  type Fanned () InputsCmd = Inputs
  fanInput eIn =
    Inputs <$>
      fanE _Open eIn <*>
      fanE _Message eIn <*>
      fanE _LimitUp eIn <*>
      fanE _LimitDown eIn <*>
      fanE _Help eIn <*>
      fanE _Quit eIn <*>
      fanE _Unknown eIn

data OutputsCmd =
    Write String
  | Close
  deriving (Eq, Ord, Show)

makePrisms ''OutputsCmd

instance Mergable () OutputsCmd where
  type ToMerge () OutputsCmd = Outputs
  mergeOutput _ (Outputs eWrite eClose) =
    foldr (unionWith combineResult) never $
      (fmap (mergeE (pure ()) _Write) eWrite) ++
      (fmap (mergeE (pure ()) _Close) eClose)
    {-
    unionWith combineResult
      (fmap (mergeE (pure ()) _Write) eWrite)
      (fmap (mergeE (pure ()) _Close) eClose)
    -}
