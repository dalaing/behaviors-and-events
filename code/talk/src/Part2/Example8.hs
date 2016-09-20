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

import Part2.Common

data Inputs = Inputs {
    ieOpen           :: Event ()
  , ieMessage        :: Event String
  , ieHelp           :: Event ()
  , ieUnknownCommand :: Event String
  , ieQuit           :: Event ()
  }

type Message = String
type Command = String

command :: String -> Either Message Command
command ('/':xs) = Right xs
command xs       = Left xs

fanOut :: InputIO -> Inputs
fanOut (InputIO eOpen eRead) =
  let
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "help")  eCommand
    eQuit    =   () <$ filterE (== "quit")  eCommand

    commands = ["help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand
  in
    Inputs eOpen eMessage eHelp eUnknown eQuit

data Outputs = Outputs {
    oeWrite :: [Event String]
  , oeClose :: [Event ()]
  }

fanIn :: Outputs -> OutputIO
fanIn (Outputs eWrites eCloses) =
  let
    addLine x y = x ++ '\n' : y
    eCombinedWrites = foldr (unionWith addLine) never eWrites
    eCombinedCloses = () <$ leftmost eCloses
  in
    OutputIO eCombinedWrites eCombinedCloses

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
  o <- networkDescription'' . fanOut $ i
  return $ fanIn o

networkDescription'' :: MonadMoment m => Inputs -> m Outputs
networkDescription'' (Inputs eOpen eMessage eHelp eUnknown eQuit) =
  let
    eWrites = [
        "Hi"           <$  eOpen
      ,                    eMessage
      , helpMessage    <$  eHelp
      , unknownMessage <$> eUnknown
      , "Bye"          <$  eQuit
      ]
    eQuits = [
        eQuit
      ]
  in
    return $ Outputs eWrites eQuits

go_2_8 :: IO ()
go_2_8 =
  mkGo networkDescription
