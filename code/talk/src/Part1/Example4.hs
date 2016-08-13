{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part1.Example4 (
    go_1_4
  , myTestableNetwork
  , InputIO(..)
  , OutputIO(..)
  , example1
  ) where

import           Control.Monad              (forever)
import           Data.Foldable              (traverse_)
import           System.Exit                (exitSuccess)

import           Reactive.Banana
import           Reactive.Banana.Frameworks

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

setupInput :: IO (EventSource String)
setupInput =
  mkEventSource

data InputIO =
  Read String
  deriving (Eq, Ord, Show)

inputToRead :: Event InputIO -> Event String
inputToRead eIn =
    filterJust $ maybeRead <$> eIn
  where
    maybeRead (Read x) = Just x
    maybeRead _ = Nothing

data OutputIO =
    Write String
  | Close
  deriving (Eq, Ord, Show)

handleInput :: EventSource String -> MomentIO (Event InputIO)
handleInput i = do
  eRead <- fromAddHandler . addHandler $ i
  return $ Read <$> eRead

data Inputs = Inputs {
    ieMessage        :: Event String
  , ieQuit           :: Event ()
  , ieUnknownCommand :: Event String
  }

fanOutOld :: Event InputIO -> Inputs
fanOutOld eIn =
  let
    eRead = inputToRead eIn
    eMessage = filterE (/= "/quit") eRead
    eQuit = () <$ filterE (== "/quit") eRead
  in
    Inputs eMessage eQuit never

data FanOutResults =
    FrMessage String
  | FrQuit

collectFanOutResults :: Inputs -> Event FanOutResults
collectFanOutResults (Inputs eMessage eQuit _) =
  leftmost [
      FrMessage <$> eMessage
    , FrQuit <$ eQuit
    ]

fanOut :: Event InputIO -> Inputs
fanOut eIn =
  let
    eRead =
      filterE (not . null) $ inputToRead eIn

    isMessage =
      (/= "/") . take 1
    eMessage =
      filterE isMessage eRead

    isCommand =
      (== "/") . take 1
    eCommand =
      fmap (drop 1) . filterE isCommand $ eRead

    eQuit =
      () <$ filterE (== "quit") eCommand
    eUnknownCommand =
      filterE (/= "quit") eCommand
  in
    Inputs eMessage eQuit eUnknownCommand

data Outputs = Outputs {
    oeWrite :: [Event String]
  , oeClose :: [Event ()]
  }

fanIn :: Outputs -> Event [OutputIO]
fanIn (Outputs eWrites eCloses) =
  let
    eCombinedWrites = fmap (\x xs -> Write x : xs) <$> eWrites
    eCombinedCloses = [(Close :) <$ leftmost eCloses]
  in
    fmap ($ []) .
    unions $
    eCombinedWrites ++ eCombinedCloses

myLogicalNetwork :: Inputs -> Outputs
myLogicalNetwork (Inputs eMessage eQuit eUnknownCommand) =
  let
    eWrites = [
        eMessage
      , "Bye" <$ eQuit
      , ("Unknown command: " ++) <$> eUnknownCommand
      ]
    eQuits = [
        eQuit
      ]
  in
    Outputs eWrites eQuits

myTestableNetwork :: Event InputIO -> Moment (Event [OutputIO])
myTestableNetwork =
  return . fanIn . myLogicalNetwork . fanOut

data MessageInput = MessageInput {
    mieRead :: Event String
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  }

handleMessage :: MessageInput -> MessageOutput
handleMessage (MessageInput eMessage) =
  MessageOutput eMessage

data QuitInput = QuitInput {
    qieQuit :: Event ()
  }

data QuitOutput = QuitOutput {
    qoeWrite :: Event String
  , qoeQuit  :: Event ()
  }

handleQuit :: QuitInput -> QuitOutput
handleQuit (QuitInput eQuit) =
  QuitOutput ("Bye" <$ eQuit) eQuit

data UnknownCommandInput = UnknownCommandInput {
    ucieUnknownCommand :: Event String
  }

data UnknownCommandOutput = UnknownCommandOutput {
    ucoeUnknownCommand :: Event String
  }

handleUnknownCommand :: UnknownCommandInput -> UnknownCommandOutput
handleUnknownCommand (UnknownCommandInput eUnknownCommand) =
  UnknownCommandOutput (("Unknown command: " ++) <$> eUnknownCommand )

myLogicalNetwork2 :: Inputs -> Outputs
myLogicalNetwork2 (Inputs eMessage eQuit eUnknownCommand) =
  let
    MessageOutput emWrite = handleMessage $ MessageInput eMessage
    QuitOutput eqWrite eqQuit = handleQuit $ QuitInput eQuit
    UnknownCommandOutput eucWrite = handleUnknownCommand $ UnknownCommandInput eUnknownCommand
  in
    Outputs [emWrite, eqWrite, eucWrite] [eqQuit]

myTestableNetwork2 :: Event InputIO -> Moment (Event [OutputIO])
myTestableNetwork2 =
  return . fanIn . myLogicalNetwork2 . fanOut

example1 :: [Maybe InputIO]
example1 = [Just (Read "one"), Nothing, Just (Read "two"), Just (Read "/quit")]

handleOutput :: OutputIO -> IO ()
handleOutput (Write s) = putStrLn s
handleOutput Close = exitSuccess

networkDescription :: EventSource String -> MomentIO ()
networkDescription s = do
  i <- handleInput s
  o <- liftMoment $ myTestableNetwork i
  reactimate $ traverse_ handleOutput <$> o

eventLoop :: EventSource String -> IO ()
eventLoop s =
  forever $ do
    x <- getLine
    fire s x

go_1_4 :: IO ()
go_1_4 = do
  input <- setupInput
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
