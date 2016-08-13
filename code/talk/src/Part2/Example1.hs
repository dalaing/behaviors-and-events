{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Example1 (
    go_2_1
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

data MessageInput = MessageInput {
    mieRead :: Event String
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  , mobLines :: Behavior Int
  }

handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  return $ MessageOutput eMessage bLines

-- TODO break up into all the different versions from the blog
handleMessage1 :: MessageInput -> Moment MessageOutput
handleMessage1 (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    f l m = m ++ " (last message: " ++ l ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut (pure 0)

handleMessage2 :: MessageInput -> Moment MessageOutput
handleMessage2 (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    bLastLength = length <$> bMessages
    f l m = m ++ " (last message length: " ++ show l ++ ")"
    eOut = f <$> bLastLength <@> eMessage
  return $ MessageOutput eOut (pure 0)

handleMessage3 :: MessageInput -> Moment MessageOutput
handleMessage3 (MessageInput eMessage) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  bMessages <- stepper "" eMessage
  let
    bLinesNow = (+ 1) <$> bLines
    bLastLength = length <$> bMessages
    f c l m = m ++ " (line count: " ++ show c ++ ", last message length: " ++ show l ++ ")"
    eOut = f <$> bLinesNow <*> bLastLength <@> eMessage
  return $ MessageOutput eOut bLines

handleMessage4 :: MessageInput -> Moment MessageOutput
handleMessage4 (MessageInput eMessage) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    f m (c, _) = (c + 1, m)
  ePair <- accumE (0, "") (f <$> eMessage)
  let
    g (c, m) = "(" ++ show c ++ ") " ++ m
    eOut = g <$> ePair
  return $ MessageOutput eOut bLines

handleMessage5 :: MessageInput -> Moment MessageOutput
handleMessage5 (MessageInput eMessage) = do
  let
    f m (c, _) = (c + 1, m)
  (ePair, bPair) <- mapAccum (0, "") . fmap (\f x -> (f x, f x)) $ (f <$> eMessage)
  let
    g (c, m) = "(" ++ show c ++ ") " ++ m
    eOut = g <$> ePair
    bLines = fst <$> bPair
  return $ MessageOutput eOut bLines

data QuitInput = QuitInput {
    qieQuit :: Event ()
  , qibLines :: Behavior Int
  }

data QuitOutput = QuitOutput {
    qoeWrite :: Event String
  , qoeQuit  :: Event ()
  }

handleQuit :: QuitInput -> QuitOutput
handleQuit (QuitInput eQuit bLines) =
  QuitOutput ((\x -> "Bye (" ++ show x ++ " messages sent)") <$> bLines <@ eQuit) eQuit

data UnknownCommandInput = UnknownCommandInput {
    ucieUnknownCommand :: Event String
  }

data UnknownCommandOutput = UnknownCommandOutput {
    ucoeUnknownCommand :: Event String
  }

handleUnknownCommand :: UnknownCommandInput -> UnknownCommandOutput
handleUnknownCommand (UnknownCommandInput eUnknownCommand) =
  UnknownCommandOutput (("Unknown command: " ++) <$> eUnknownCommand )

myLogicalNetwork :: Inputs -> Moment Outputs
myLogicalNetwork (Inputs eMessage eQuit eUnknownCommand) = do
  MessageOutput emWrite bLines <- handleMessage $ MessageInput eMessage
  let
    QuitOutput eqWrite eqQuit = handleQuit $ QuitInput eQuit bLines
    UnknownCommandOutput eucWrite = handleUnknownCommand $ UnknownCommandInput eUnknownCommand
  return $ Outputs [emWrite, eqWrite, eucWrite] [eqQuit]

myTestableNetwork :: Event InputIO -> Moment (Event [OutputIO])
myTestableNetwork eIn = do
  n <- liftMoment . myLogicalNetwork . fanOut $ eIn
  return $ fanIn n

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

go_2_1 :: IO ()
go_2_1 = do
  input <- setupInput
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
