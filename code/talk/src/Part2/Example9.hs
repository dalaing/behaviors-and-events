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

data OpenInput  = OpenInput  { oieOpen :: Event () }
data OpenOutput = OpenOutput { ooeWrite :: Event String }

handleOpen :: MonadMoment m => OpenInput -> m OpenOutput
handleOpen (OpenInput eOpen) =
  let
    eWrite = "Hi" <$ eOpen
  in
    return $ OpenOutput eWrite

data MessageInput  = MessageInput  { mieRead :: Event String }
data MessageOutput = MessageOutput { moeWrite :: Event String }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) =
  return $ MessageOutput eMessage

data HelpInput  = HelpInput  { hieHelp :: Event () }
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
data UnknownOutput = UnknownOutput { ucoeWrite :: Event String }

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
  o <- networkDescription'' . handleIOInput $ i
  return $ handleIOOutput o

networkDescription'' :: Inputs -> Moment Outputs
networkDescription'' (Inputs eOpen (ReadInputs eMessage eHelp eUnknown eQuit)) = do
  OpenOutput eoWrite        <- handleOpen    $ OpenInput eOpen
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite        <- handleHelp    $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit    $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown

  let
    writes  =
      WriteOutputs eoWrite emWrite ehWrite euWrite eqWrite

  return $ Outputs writes eQuit

go_2_9 :: IO ()
go_2_9 =
  mkGo networkDescription
