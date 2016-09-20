{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Example9 (
    go_2_9
  ) where

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import Part2.Common

data Inputs = Inputs {
    ieOpen    :: Event ()
  , ieMessage :: Event String
  , ieHelp    :: Event ()
  , ieUnknown :: Event String
  , ieQuit    :: Event ()
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

data OpenInput  = OpenInput  { oieOpen  :: Event () }
data OpenOutput = OpenOutput { ooeWrite :: Event String }

handleOpen :: MonadMoment m => OpenInput -> m OpenOutput
handleOpen (OpenInput eOpen) =
  let
    eWrite = "Hi" <$ eOpen
  in
    return $ OpenOutput eWrite

data MessageInput  = MessageInput  { mieRead  :: Event String }
data MessageOutput = MessageOutput { moeWrite :: Event String }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) =
  return $ MessageOutput eMessage

data HelpInput  = HelpInput  { hieHelp  :: Event () }
data HelpOutput = HelpOutput { hoeWrite :: Event String }

helpMessage :: String
helpMessage =
  "/help              - displays this message\n" ++
  "/quit              - exits the program"

handleHelp :: MonadMoment m => HelpInput -> m HelpOutput
handleHelp (HelpInput eHelp) =
  let
    eWrite = helpMessage <$ eHelp
  in
    return $ HelpOutput eWrite

data QuitInput = QuitInput {
    qieQuit :: Event ()
  }

data QuitOutput = QuitOutput {
    qoeWrite :: Event String
  , qoeQuit  :: Event ()
  }

handleQuit :: MonadMoment m => QuitInput -> m QuitOutput
handleQuit (QuitInput eQuit) =
  let
    eWrite = "Bye" <$ eQuit
  in
    return $ QuitOutput eWrite eQuit

data UnknownInput  = UnknownInput  { ucieCommand :: Event String }
data UnknownOutput = UnknownOutput { ucoeWrite   :: Event String }

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

handleUnknown :: MonadMoment m => UnknownInput -> m UnknownOutput
handleUnknown (UnknownInput eUnknown) =
   return . UnknownOutput $ unknownMessage <$> eUnknown

networkDescription :: InputSources -> MomentIO ()
networkDescription =
  mkNetwork networkDescription'

networkDescription' :: InputIO -> Moment OutputIO
networkDescription' i = do
  o <- networkDescription'' . fanOut $ i
  return $ fanIn o

networkDescription'' :: Inputs -> Moment Outputs
networkDescription'' (Inputs eOpen eMessage eHelp eUnknown eQuit) = do
  OpenOutput eoWrite        <- handleOpen    $ OpenInput eOpen
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite        <- handleHelp    $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit    $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown

  return $ Outputs [eoWrite, emWrite, ehWrite, eqWrite, euWrite] [eqQuit]

go_2_9 :: IO ()
go_2_9 =
  mkGo networkDescription
