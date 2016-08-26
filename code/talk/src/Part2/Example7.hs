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

import           Part2.Common

pureNetworkDescription :: InputIO -> Moment OutputIO
pureNetworkDescription (InputIO eOpen eRead) =
  let
    eMessage = filterE ((/= "/") . take 1) eRead
    eCommand = fmap (drop 1) . filterE ((== "/") . take 1) $ eRead
    eHelp    = () <$ filterE (== "help") eCommand
    eQuit    = () <$ filterE (== "quit") eCommand

    commands        = ["help", "quit"]
    eUnknownCommand = filterE (`notElem` commands) eCommand

    eWrite = leftmost [
        "Hi (type /help for instructions)" <$ eOpen
      , eMessage
      , "/help displays this message\n/quit exits the program" <$ eHelp
      , "Bye" <$ eQuit
      , (\x -> "Unknown command: " ++ x ++ " (type /help for instructions)") <$> eUnknownCommand
      ]
  in
    return $ OutputIO eWrite eQuit

go_2_7 :: IO ()
go_2_7 =
  mkGo pureNetworkDescription
