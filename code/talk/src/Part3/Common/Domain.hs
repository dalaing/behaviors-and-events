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
  , ReadInputs(..)
  , Outputs(..)
  , WriteOutputs(..)
  , handleIO
  , InputsCmd(..)
  , OutputsCmd(..)
  ) where

import Control.Lens

import Reactive.Banana

import Part3.Common.Util
import Part3.Common.IO
import Part3.Common.Testing

data ReadInputs = ReadInputs {
    rieMessage        :: Event String
  , rieLimitUp        :: Event ()
  , rieLimitDown      :: Event ()
  , rieHelp           :: Event ()
  , rieUnknownCommand :: Event String
  , rieQuit           :: Event ()
  }

data Inputs = Inputs {
    ieOpen :: Event ()
  , iReads :: ReadInputs
  }

command :: String -> Either String String
command ('/':xs) = Right xs
command xs       = Left xs

fanReads :: Event String -> ReadInputs
fanReads eRead =
  let
    (eMessage, eCommand) = split $ command <$> eRead

    eLimitUp   =   () <$ filterE (== "limitup")   eCommand
    eLimitDown =   () <$ filterE (== "limitdown") eCommand
    eHelp      =   () <$ filterE (== "help")      eCommand
    eQuit      =   () <$ filterE (== "quit")      eCommand

    commands = ["limitup", "limitdown", "help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand
  in
    ReadInputs eMessage eLimitUp eLimitDown eHelp eUnknown eQuit

handleIOInput :: InputIO -> Inputs
handleIOInput (InputIO eOpen eRead) =
  Inputs eOpen (fanReads eRead)

data WriteOutputs = WriteOutputs {
    woeOpen    :: Event String
  , woeMessage :: Event String
  , woeHelp    :: Event String
  , woeUnknown :: Event String
  , woeQuit    :: Event String
  }

data Outputs = Outputs {
    oWrites :: WriteOutputs
  , oeClose :: Event ()
  }

mergeWrites :: WriteOutputs -> Event String
mergeWrites (WriteOutputs eOpen eMessage eHelp eUnknown eQuit) =
  let
    addLine x y = x ++ '\n' : y
    eCombinedWrites = foldr (unionWith addLine) never [
        eOpen
      , eMessage
      , eHelp
      , eUnknown
      , eQuit
      ]
  in
    eCombinedWrites

handleIOOutput :: Outputs -> OutputIO
handleIOOutput (Outputs writes eClose) =
  OutputIO (mergeWrites writes) eClose

handleIO :: MonadMoment m => (Inputs -> m Outputs) -> InputIO -> m OutputIO
handleIO n i = do
  o <- n . handleIOInput $ i
  return $ handleIOOutput o

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
    let
      readInputs =
        ReadInputs <$>
          fanE _Message eIn <*>
          fanE _LimitUp eIn <*>
          fanE _LimitDown eIn <*>
          fanE _Help eIn <*>
          fanE _Unknown eIn <*>
          fanE _Quit eIn
    in
      Inputs <$>
        fanE _Open eIn <*>
        readInputs

data OutputsCmd =
    Write String
  | Close
  deriving (Eq, Ord, Show)

makePrisms ''OutputsCmd

instance Mergable () OutputsCmd where
  type ToMerge () OutputsCmd = Outputs
  mergeOutput _ (Outputs (WriteOutputs eoWrite emWrite ehWrite euWrite eqWrite) eClose) =
    foldr (unionWith combineResult) never $
      mergeE (pure ()) _Close eClose :
      (mergeE (pure()) _Write <$> [eoWrite, emWrite, ehWrite, euWrite, eqWrite])
