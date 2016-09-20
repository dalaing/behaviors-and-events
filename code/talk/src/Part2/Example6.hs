{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Example6 (
    go_2_6
  ) where

import Control.Monad (forever)
import System.Exit (exitSuccess)

import Reactive.Banana
import Reactive.Banana.Frameworks

orElse :: Event a -> Event a -> Event a
orElse = unionWith const

leftmost :: [Event a] -> Event a
leftmost = foldl orElse never

data EventSource a = EventSource {
    addHandler :: AddHandler a
  , fire       :: a -> IO ()
  }

mkEventSource :: IO (EventSource a)
mkEventSource =
  uncurry EventSource <$> newAddHandler

data InputSources = InputSources {
    isOpen :: EventSource ()
  , isRead :: EventSource String
  }

mkInputSources :: IO InputSources
mkInputSources =
  InputSources <$> mkEventSource <*> mkEventSource

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
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "help")  eCommand
    eQuit    =   () <$ filterE (== "quit")  eCommand

    commands = ["help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand

    eWrite = leftmost [
        "Hi"                  <$  eOpen
      ,                           eMessage
      , helpMessage           <$  eHelp
      , unknownMessage        <$> eUnknown
      , "Bye"                 <$  eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$ eQuit

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

go_2_6 :: IO ()
go_2_6 = do
  input <- mkInputSources
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
