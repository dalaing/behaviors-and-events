{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
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
    ieMessage          :: Event String
  , ieHistoryLimitUp   :: Event ()
  , ieHistoryLimitDown :: Event ()
  , ieQuit             :: Event ()
  , ieUnknownCommand   :: Event String
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

    commands =
     ["limitup", "limitdown", "quit"]

    [eHistoryLimitUp, eHistoryLimitDown, eQuit] =
      fmap (\x -> () <$ filterE (== x) eCommand) commands

    eUnknownCommand =
      filterE (`notElem` commands) eCommand

{-
    eHistoryLimitUp =
      () <$ filterE (== "limitup") eCommand
    eHistoryLimitUp =
      () <$ filterE (== "limitdown") eCommand
    eQuit =
      () <$ filterE (== "quit") eCommand

    eUnknownCommand =
      filterE (`notElem` ["limitup", "limitdown", "quit"]) eCommand
-}
  in
    Inputs eMessage eHistoryLimitUp eHistoryLimitDown eQuit eUnknownCommand

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

data HistoryLimitInput = HistoryLimitInput {
    hlieLimitUp :: Event ()
  , hlieLimitDown :: Event ()
  }

data HistoryLimitOutput = HistoryLimitOutput {
    hloeLimit :: Event Int
  , hlobLimit :: Behavior Int
  }

handleHistoryLimit :: MonadMoment m => HistoryLimitInput -> m HistoryLimitOutput
handleHistoryLimit (HistoryLimitInput eUp eDown) = do
  (eLimit, bLimit) <- mapAccum 1 . fmap (\f x -> (f x, f x)) . unions $ [
      (+ 1) <$ eUp
    , (max 0 . subtract 1) <$ eDown
    ]
  return $ HistoryLimitOutput eLimit bLimit

data HistoryInput = HistoryInput {
    hieMessage :: Event String
  , hieLimit :: Event Int
  , hibLimit :: Behavior Int
  }

data HistoryOutput = HistoryOutput {
    -- possibly want behavior of history as well
    -- hoeHistory :: Event [String]
    hoeWrite :: Event String
  }

data History = History {
    hLimit :: Int
  , hMessages :: [String]
  }

changeLimit :: Int -> History -> History
changeLimit n (History _ ms) =
  History n (take n ms)

addMessage :: String -> History -> History
addMessage m (History n ms) =
  History n (take n $ m : ms)

handleHistory :: MonadMoment m => HistoryInput -> m HistoryOutput
handleHistory (HistoryInput eMessage eLimit _) = do
  bHistory <- accumB (History 1 []) . unions $ [
      changeLimit <$> eLimit
    , addMessage <$> eMessage
    ]
  let
    f (History l ms) = "(last " ++ show l ++ "message: " ++ show ms ++ ")"
    eWrite = f <$> bHistory <@ eMessage
  return $ HistoryOutput eWrite

addMessage2 :: Int -> String -> [String] -> [String]
addMessage2 n m ms =
  take n (m : ms)

handleHistory2 :: MonadMoment m => HistoryInput -> m HistoryOutput
handleHistory2 (HistoryInput eMessage eLimit bLimit) = do
  bHistory <- accumB [] . unions $ [
      take <$> eLimit
    , addMessage2 <$> bLimit <@> eMessage
    ]
  let
    f l h = "(last " ++ show l ++ "message: " ++ show h ++ ")"
    eWrite = f <$> bLimit <*> bHistory <@ eMessage
  return $ HistoryOutput eWrite

data MessageInput = MessageInput {
    mieRead :: Event String
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  , mobLines :: Behavior Int
  }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  return $ MessageOutput eMessage bLines

-- TODO break up into all the different versions from the blog
handleMessage1 :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage1 (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    f l m = m ++ " (last message: " ++ l ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut (pure 0)

handleMessage2 :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage2 (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    bLastLength = length <$> bMessages
    f l m = m ++ " (last message length: " ++ show l ++ ")"
    eOut = f <$> bLastLength <@> eMessage
  return $ MessageOutput eOut (pure 0)

handleMessage3 :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage3 (MessageInput eMessage) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    f ms m = m ++ " (last messages: " ++ show ms ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut (pure 0)

handleMessage4 :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage4 (MessageInput eMessage) = do
  let
    limitCons n x xs = take n (x : xs)
  bMessages <- accumB [] . fmap (limitCons 3) $ eMessage
  let
    f ms m = m ++ " (last 3 messages: " ++ show ms ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut (pure 0)

handleMessage5 :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage5 (MessageInput eMessage) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    bLimit = pure 3
    bLimitedMessages = take <$> bLimit <*> bMessages
    f ms m = m ++ " (last 3 messages: " ++ show ms ++ ")"
    eOut = f <$> bLimitedMessages <@> eMessage
  return $ MessageOutput eOut (pure 0)

handleMessage6 :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage6 (MessageInput eMessage) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    bLimit = pure 3
    bLimitedMessages = take <$> bLimit <*> bMessages
    f l ms m = m ++ " (last " ++ show l ++ " messages: " ++ show ms ++ ")"
    eOut = f <$> bLimit <*> bLimitedMessages <@> eMessage
  return $ MessageOutput eOut (pure 0)

handleMessage7 :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage7 (MessageInput eMessage) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  bMessages <- stepper "" eMessage
  let
    bLinesNow = (+ 1) <$> bLines
    bLastLength = length <$> bMessages
    f c l m = m ++ " (line count: " ++ show c ++ ", last message length: " ++ show l ++ ")"
    eOut = f <$> bLinesNow <*> bLastLength <@> eMessage
  return $ MessageOutput eOut bLines

handleMessage8 :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage8 (MessageInput eMessage) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    f m (c, _) = (c + 1, m)
  ePair <- accumE (0, "") (f <$> eMessage)
  let
    g (c, m) = "(" ++ show c ++ ") " ++ m
    eOut = g <$> ePair
  return $ MessageOutput eOut bLines

handleMessage9 :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage9 (MessageInput eMessage) = do
  let
    f m (c, _) = (c + 1, m)
  (ePair, bPair) <- mapAccum (0, "") . fmap (\f x -> (f x, f x)) $ (f <$> eMessage)
  let
    g (c, m) = "(" ++ show c ++ ") " ++ m
    eOut = g <$> ePair
    bLines = fst <$> bPair
  return $ MessageOutput eOut bLines

data QuitInput = QuitInput {
    qieQuit  :: Event ()
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

myLogicalNetwork :: MonadMoment m => Inputs -> m Outputs
myLogicalNetwork (Inputs eMessage eLimitUp eLimitDown eQuit eUnknownCommand) = do
  HistoryLimitOutput eLimit bLimit <- handleHistoryLimit $ HistoryLimitInput eLimitUp eLimitDown
  HistoryOutput ehWrite <- handleHistory $ HistoryInput eMessage eLimit bLimit
  MessageOutput emWrite bLines <- handleMessage $ MessageInput eMessage
  let
    QuitOutput eqWrite eqQuit = handleQuit $ QuitInput eQuit bLines
    UnknownCommandOutput eucWrite = handleUnknownCommand $ UnknownCommandInput eUnknownCommand
  return $ Outputs [emWrite, ehWrite, eqWrite, eucWrite] [eqQuit]

myTestableNetwork :: MonadMoment m => Event InputIO -> m (Event [OutputIO])
myTestableNetwork eIn = do
  n <- myLogicalNetwork . fanOut $ eIn
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
