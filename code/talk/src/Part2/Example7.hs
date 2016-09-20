{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Example7 (
    go_2_7
  ) where

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Part2.Common

helpMessage :: String
helpMessage =
  "/help              - displays this message\n" ++
  "/quit              - exits the program"

type Message = String
type Command = String

command :: String -> Either Message Command
command ('/':xs) = Right xs
command xs       = Left xs

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
networkDescription' (InputIO eOpen eRead) =
  let
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "help")  eCommand
    eQuit    =   () <$ filterE (== "quit")  eCommand

    commands = ["help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand

    eWrite = leftmost [
        "Hi"           <$  eOpen
      ,                    eMessage
      , helpMessage    <$  eHelp
      , unknownMessage <$> eUnknown
      , "Bye"          <$  eQuit
      ]
  in
    return $ OutputIO eWrite eQuit

go_2_7 :: IO ()
go_2_7 =
  mkGo networkDescription
