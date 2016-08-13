{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Example2 (
    go_2_2
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
  , ieUpgrade        :: Event ()
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

    eUpgrade =
      () <$ filterE (== "upgrade") eCommand
    eQuit =
      () <$ filterE (== "quit") eCommand
    eUnknownCommand =
      filterE (`notElem` ["upgrade", "quit"]) eCommand
  in
    Inputs eMessage eUpgrade eQuit eUnknownCommand

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
  }

handleMessage :: MessageInput -> MessageOutput
handleMessage (MessageInput eMessage) =
  MessageOutput eMessage

data AccountType =
    Plebian
  | Premium
  deriving (Eq, Ord, Show)

softUpgradeCheck :: AccountType -> Int -> Bool
softUpgradeCheck Plebian x = x `mod` 5 == 4
softUpgradeCheck Premium x = False

hardUpgradeCheck :: AccountType -> Int -> Bool
hardUpgradeCheck Plebian x = x >= 9
hardUpgradeCheck Premium x = False

data UpgradeInput = UpgradeInput {
    uieUpgrade :: Event ()
  }

data UpgradeOutput = UpgradeOutput {
    uobAccount :: Behavior AccountType
  }

upgrade :: UpgradeInput -> Moment UpgradeOutput
upgrade (UpgradeInput eUpgrade) = do
  bFns <- switchB (pure Plebian) (pure Premium <$ eUpgrade)
  return $ UpgradeOutput bFns

data QuitInput = QuitInput {
    qieQuit :: Event ()
  }

data QuitOutput = QuitOutput {
    qoeWrite :: Event String
  , qoeQuit  :: Event ()
  }

handleQuit :: QuitInput -> QuitOutput
handleQuit (QuitInput eQuit) =
  let
    eOut = "Bye" <$ eQuit
  in
    QuitOutput eOut eQuit

data UnknownCommandInput = UnknownCommandInput {
    ucieUnknownCommand :: Event String
  }

data UnknownCommandOutput = UnknownCommandOutput {
    ucoeUnknownCommand :: Event String
  }

handleUnknownCommand :: UnknownCommandInput -> UnknownCommandOutput
handleUnknownCommand (UnknownCommandInput eUnknownCommand) =
  UnknownCommandOutput (("Unknown command: " ++) <$> eUnknownCommand )

data CounterInput = CounterInput {
    cieMessage :: Event String
  , cieQuit    :: Event ()
  , cibAccount :: Behavior AccountType
  }

data CounterOutput = CounterOutput {
    coeMessage :: Event String
  , coeQuit    :: Event ()
  }

counter1 :: CounterInput -> Moment CounterOutput
counter1 (CounterInput eMessage eQuit _) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    f l = show l ++ " messages sent"
    eOut = f <$> bLines <@ eQuit
  return $ CounterOutput eOut never

counter2 :: CounterInput -> Moment CounterOutput
counter2 (CounterInput eMessage eQuit _) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimitReached = (\x -> x `mod` 5 == 4) <$> bLines
    eSoftLimitReached = whenE bSoftLimitReached eMessage
    bHardLimitReached = (>= 9) <$> bLines
    eHardLimitReached = whenE bHardLimitReached eMessage
    eSoftMessage = "You are using a Plebian account, consider upgrading" <$ eSoftLimitReached
    eHardMessage = "You have reached your message limit for a Plebian account, consider upgrading" <$ eHardLimitReached
    eMessageOut = leftmost [eHardMessage, eSoftMessage]
    eQuitOut = () <$ eHardLimitReached
  return $ CounterOutput eMessageOut eQuitOut

counter3 :: CounterInput -> Moment CounterOutput
counter3 (CounterInput eMessage eQuit _) = do
  let
    bSoftLimit = pure 5
    bHardLimit = pure 10
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimitReached = (\n x -> x `mod` n == (n - 1)) <$> bSoftLimit <*> bLines
    eSoftLimitReached = whenE bSoftLimitReached eMessage
    bHardLimitReached = (\n x -> x >= (n - 1)) <$> bHardLimit <*> bLines
    eHardLimitReached = whenE bHardLimitReached eMessage
    eSoftMessage = "You are using a Plebian account, consider upgrading" <$ eSoftLimitReached
    eHardMessage = "You have reached your message limit for a Plebian account, consider upgrading" <$ eHardLimitReached
    eMessageOut = leftmost [eHardMessage, eSoftMessage]
    eQuitOut = () <$ eHardLimitReached
  return $ CounterOutput eMessageOut eQuitOut

counter4 :: CounterInput -> Moment CounterOutput
counter4 (CounterInput eMessage eQuit _) = do
  let
    bSoftLimit = pure $ \x -> x `mod` 5 == 4
    bHardLimit = pure $ \x -> x >= 9
  eLines <- accumE (-1) ((+ 1) <$ eMessage)
  let
    eSoftLimitReached = filterApply bSoftLimit eLines
    eHardLimitReached = filterApply bHardLimit eLines
    eSoftMessage = "You are using a Plebian account, consider upgrading" <$ eSoftLimitReached
    eHardMessage = "You have reached your message limit for a Plebian account, consider upgrading" <$ eHardLimitReached
    eMessageOut = leftmost [eHardMessage, eSoftMessage]
    eQuitOut = () <$ eHardLimitReached
  return $ CounterOutput eMessageOut eQuitOut

counter5 :: CounterInput -> Moment CounterOutput
counter5 (CounterInput eMessage eQuit bAccount) = do
  let
    bSoftLimit = softUpgradeCheck <$> bAccount
    bHardLimit = hardUpgradeCheck <$> bAccount
  eLines <- accumE (-1) ((+ 1) <$ eMessage)
  let
    eSoftLimitReached = filterApply bSoftLimit eLines
    eHardLimitReached = filterApply bHardLimit eLines
    eSoftMessage = "You are using a Plebian account, consider upgrading" <$ eSoftLimitReached
    eHardMessage = "You have reached your message limit for a Plebian account, consider upgrading" <$ eHardLimitReached
    eMessageOut = leftmost [eHardMessage, eSoftMessage]
    eQuitOut = () <$ eHardLimitReached
  return $ CounterOutput eMessageOut eQuitOut

-- TODO a version where the counter function is what get switched

myLogicalNetwork :: Inputs -> Moment Outputs
myLogicalNetwork (Inputs eMessage eUpgrade eQuit eUnknownCommand) = do
  UpgradeOutput bAccount <- upgrade $ UpgradeInput eUpgrade
  CounterOutput ecWrite ecQuit <- counter5 $ CounterInput eMessage eQuit bAccount
  let
    MessageOutput emWrite = handleMessage $ MessageInput eMessage
    QuitOutput eqWrite eqQuit = handleQuit $ QuitInput eQuit
    UnknownCommandOutput eucWrite = handleUnknownCommand $ UnknownCommandInput eUnknownCommand
  return $ Outputs [emWrite, ecWrite, eqWrite, eucWrite] [eqQuit, ecQuit]

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

go_2_2 :: IO ()
go_2_2 = do
  input <- setupInput
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
