{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
module Part3.Common.Domain (
    Inputs(..)
  , Outputs(..)
  , liftDomainNetwork
  , InputsCmd(..)
  , OutputsCmd(..)
  ) where

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

data OutputsCmd =
    Write String
  | Close
  deriving (Eq, Ord, Show)

instance Fannable Inputs where
  type ToFan Inputs = InputsCmd
  fanInput eIn =
    let
      maybeOpen Open = Just ()
      maybeOpen _      = Nothing
      eOpen = filterJust $ maybeOpen <$> eIn

      maybeMessage (Message x) = Just x
      maybeMessage _             = Nothing
      eMessage = filterJust $ maybeMessage <$> eIn

      maybeLimitUp LimitUp = Just ()
      maybeLimitUp _      = Nothing
      eLimitUp = filterJust $ maybeLimitUp <$> eIn

      maybeLimitDown LimitDown = Just ()
      maybeLimitDown _      = Nothing
      eLimitDown = filterJust $ maybeLimitDown <$> eIn

      maybeHelp Help = Just ()
      maybeHelp _      = Nothing
      eHelp = filterJust $ maybeHelp <$> eIn

      maybeQuit Quit = Just ()
      maybeQuit _      = Nothing
      eQuit = filterJust $ maybeQuit <$> eIn

      maybeUnknown (Unknown x) = Just x
      maybeUnknown _             = Nothing
      eUnknown = filterJust $ maybeUnknown <$> eIn
    in
      return $ Inputs eOpen eMessage eLimitUp eLimitDown eHelp eQuit eUnknown

instance Mergable Outputs where
  type Merged Outputs = OutputsCmd
  mergeOutput _ (Outputs eWrite eClose) =
    foldr (unionWith (++)) never $
      fmap (fmap (\x -> [Write x])) eWrite ++
      fmap (fmap (\_ -> [Close])) eClose
