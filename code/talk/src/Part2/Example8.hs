{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Example8 (
    go_2_8
  ) where

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Part2.Common

data ReadInputs = ReadInputs {
    rieMessage :: Event String
  , rieHelp    :: Event ()
  , rieUnknown :: Event String
  , rieQuit    :: Event ()
  }

data Inputs = Inputs {
    ieOpen :: Event ()
  , iReads :: ReadInputs
  }

type Message = String
type Command = String

command :: String -> Either Message Command
command ('/':xs) = Right xs
command xs       = Left xs

fanReads :: Event String -> ReadInputs
fanReads eRead =
  let
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "help")  eCommand
    eQuit    =   () <$ filterE (== "quit")  eCommand

    commands = ["help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand
  in
    ReadInputs eMessage eHelp eUnknown eQuit

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

helpMessage :: String
helpMessage =
  "/help              - displays this message\n" ++
  "/quit              - exits the program"

unknownMessage :: Command -> String
unknownMessage cmd =
  let
    commandError = case cmd of
      "" ->
        "Command can not be an empty string."
      cmd ->
        "Unknown command: " ++ cmd ++ "."

    helpPrompt =
      "\nType /help for options."
  in
    commandError ++ helpPrompt

networkDescription :: InputSources -> MomentIO ()
networkDescription =
  mkNetwork networkDescription'

networkDescription' :: InputIO -> Moment OutputIO
networkDescription' i = do
  o <- networkDescription'' . handleIOInput $ i
  return $ handleIOOutput o

networkDescription'' :: MonadMoment m => Inputs -> m Outputs
networkDescription'' (Inputs eOpen (ReadInputs eMessage eHelp eUnknown eQuit)) =
  let
    eoWrite =           "Hi" <$  eOpen
    emWrite =                    eMessage
    ehWrite =    helpMessage <$  eHelp
    euWrite = unknownMessage <$> eUnknown
    eqWrite =          "Bye" <$  eQuit
    writes  =
      WriteOutputs eoWrite emWrite ehWrite euWrite eqWrite
  in
    return $ Outputs writes eQuit

go_2_8 :: IO ()
go_2_8 =
  mkGo networkDescription
