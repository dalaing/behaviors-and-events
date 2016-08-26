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

import Part2.Common

data Inputs = Inputs {
    ieOpen           :: Event ()
  , ieMessage        :: Event String
  , ieHelp           :: Event ()
  , ieQuit           :: Event ()
  , ieUnknownCommand :: Event String
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

    eHelp = () <$ filterE (== "help") eCommand
    eQuit = () <$ filterE (== "quit") eCommand

    commands =
      ["help", "quit"]
    eUnknownCommand =
      filterE (`notElem` commands) eCommand
  in
    Inputs eOpen eMessage eHelp eQuit eUnknownCommand

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

domainNetworkDescription :: MonadMoment m => Inputs -> m Outputs
domainNetworkDescription (Inputs eOpen eMessage eHelp eQuit eUnknownCommand) =
  let
    eWrites = [
        "Hi (type /help for instructions)" <$ eOpen
      , eMessage
      , "/help displays this message\n/quit exits the program" <$ eHelp
      , "Bye" <$ eQuit
      , (\x -> "Unknown command: " ++ x ++ " (type /help for instructions)") <$> eUnknownCommand
      ]
    eQuits = [
        eQuit
      ]
  in
    return $ Outputs eWrites eQuits

pureNetworkDescription :: InputIO -> Moment OutputIO
pureNetworkDescription i = do
  o <- domainNetworkDescription . fanOut $ i
  return $ fanIn o

go_2_8 :: IO ()
go_2_8 =
  mkGo pureNetworkDescription
