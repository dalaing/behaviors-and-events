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

    commands =
     ["upgrade", "quit"]

    [eUpgrade, eQuit] =
      fmap (\x -> () <$ filterE (== x) eCommand) commands

    eUnknownCommand =
      filterE (`notElem` commands) eCommand
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

data UpgradeInput = UpgradeInput {
    uieUpgrade :: Event ()
  }

data UpgradeOutput = UpgradeOutput {
    uobAccount :: Behavior AccountType
  }

upgrade :: MonadMoment m => UpgradeInput -> m UpgradeOutput
upgrade (UpgradeInput eUpgrade) = do
  bFns <- stepper Plebian (Premium <$ eUpgrade)
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

data CounterInputOld = CounterInputOld {
    cioeMessage :: Event String
  , cioeQuit    :: Event ()
  }

data CounterOutputOld = CounterOutputOld {
    cooeMessage :: Event String
  }

counter1 :: MonadMoment m => CounterInputOld -> m CounterOutputOld
counter1 (CounterInputOld eMessage eQuit) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    f l = show l ++ " messages sent"
    eOut = f <$> bLines <@ eQuit
  return $ CounterOutputOld eOut

data LimitInput = LimitInput {
    lieSoftLimit :: Event ()
  , lieHardLimit :: Event ()
  }

data LimitOutput = LimitOutput {
    loeWrite :: Event String
  , loeQuit  :: Event ()
  }

handleLimit :: LimitInput -> LimitOutput
handleLimit (LimitInput eSoftLimit eHardLimit) =
  let
    eSoftLimitMessage =
      "You are using a Plebian account.  Consider upgrading to a Premium account for unlimited messages" <$ eSoftLimit
    eHardLimitMessage =
      "You have reached your message limit for a Plebian account, please upgrade" <$ eHardLimit
    eMessage =
      leftmost [
          eHardLimitMessage
        , eSoftLimitMessage
        ]
    eQuit =
      eHardLimit
  in
    LimitOutput eMessage eQuit

data CounterInput = CounterInput {
    cieMessage :: Event String
  , cibAccount :: Behavior AccountType
  }

softLimitCheck :: AccountType -> Int -> Bool
softLimitCheck Plebian x = x `mod` 5 == 4
softLimitCheck Premium x = False

hardLimitCheck :: AccountType -> Int -> Bool
hardLimitCheck Plebian x = x >= 9
hardLimitCheck Premium x = False

counter2 :: MonadMoment m => CounterInput -> m LimitInput
counter2 (CounterInput eMessage bAccount) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimitReached = softLimitCheck <$> bAccount <*> bLines
    bHardLimitReached = hardLimitCheck <$> bAccount <*> bLines

    eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
    eHardLimitReached = () <$ whenE bHardLimitReached eMessage
  return $ LimitInput eSoftLimitReached eHardLimitReached

softLimitCheck2 :: Int -> AccountType -> Int -> Bool
softLimitCheck2 n Plebian x = x `mod` n == (n - 1)
softLimitCheck2 _ Premium _ = False

hardLimitCheck2 :: Int -> AccountType -> Int -> Bool
hardLimitCheck2 n Plebian x = x >= (n - 1)
hardLimitCheck2 _ Premium _ = False

counter3 :: MonadMoment m => CounterInput -> m LimitInput
counter3 (CounterInput eMessage bAccount) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimit = pure 5
    bHardLimit = pure 10

    bSoftLimitReached = softLimitCheck2 <$> bSoftLimit <*> bAccount <*> bLines
    bHardLimitReached = hardLimitCheck2 <$> bHardLimit <*> bAccount <*> bLines

    eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
    eHardLimitReached = () <$ whenE bHardLimitReached eMessage
  return $ LimitInput eSoftLimitReached eHardLimitReached

counter4 :: MonadMoment m => CounterInput -> m LimitInput
counter4 (CounterInput eMessage bAccount) = do
  eLines <- accumE (-1) ((+ 1) <$ eMessage)
  let
    bSoftLimit = pure 5
    bHardLimit = pure 10

    bSoftLimitFn = softLimitCheck2 <$> bSoftLimit <*> bAccount
    bHardLimitFn = hardLimitCheck2 <$> bHardLimit <*> bAccount

    eSoftLimitReached = () <$ filterApply bSoftLimitFn eLines
    eHardLimitReached = () <$ filterApply bHardLimitFn eLines
  return $ LimitInput eSoftLimitReached eHardLimitReached

softLimitCheck3 :: Int -> Int -> Bool
softLimitCheck3 n x = x `mod` n == (n - 1)

hardLimitCheck3 :: Int -> Int -> Bool
hardLimitCheck3 n x = x >= (n - 1)

data LimitInput2 = LimitInput2 {
    lieUpgrade     :: Event ()
  , lieMessage     :: Event String
  , libSoftLimitFn :: Behavior (Int -> Bool)
  , libHardLimitFn :: Behavior (Int -> Bool)
  }

data LimitOutput2 = LimitOutput2 {
    lobSoftLimitReached :: Behavior Bool
  , lobHardLimitReached :: Behavior Bool
  }

plebianLimit :: MonadMoment m => LimitInput2 -> m LimitOutput2
plebianLimit (LimitInput2 _ eMessage bSoftLimitFn bHardLimitFn) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimitReached = bSoftLimitFn <*> bLines
    bHardLimitReached = bHardLimitFn <*> bLines
  return $ LimitOutput2 bSoftLimitReached bHardLimitReached

limit :: MonadMoment m => LimitInput2 -> m LimitOutput2
limit li@(LimitInput2 eUpgrade _ _ _) = do
  let
    bPremiumLimitReached = pure False
  LimitOutput2 bPlebianSoftLimitReached bPlebianHardLimitReached <- plebianLimit li
  bSoftLimitReached <- switchB bPlebianSoftLimitReached (bPremiumLimitReached <$ eUpgrade)
  bHardLimitReached <- switchB bPlebianHardLimitReached (bPremiumLimitReached <$ eUpgrade)
  return $ LimitOutput2 bSoftLimitReached bHardLimitReached

counter5 :: MonadMoment m => Event () -> CounterInput -> m LimitInput
counter5 eUpgrade (CounterInput eMessage _) = do
  LimitOutput2 bSoftLimitReached bHardLimitReached <- limit $ LimitInput2 eUpgrade eMessage (softLimitCheck3 <$> pure 5) (hardLimitCheck3 <$> pure 10)
  let
    eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
    eHardLimitReached = () <$ whenE bHardLimitReached eMessage
  return $ LimitInput eSoftLimitReached eHardLimitReached

counter6Pleb :: MonadMoment m => CounterInput -> m LimitInput
counter6Pleb (CounterInput eMessage _) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimit = pure 5
    bHardLimit = pure 10

    bSoftLimitReached = softLimitCheck3 <$> bSoftLimit <*> bLines
    bHardLimitReached = hardLimitCheck3 <$> bHardLimit <*> bLines

    eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
    eHardLimitReached = () <$ whenE bHardLimitReached eMessage
  return $ LimitInput eSoftLimitReached eHardLimitReached

counter6Prem :: MonadMoment m => CounterInput -> m LimitInput
counter6Prem _ = return $ LimitInput never never

counter6 :: MonadMoment m => Event () -> Event () -> CounterInput -> m LimitInput
counter6 eOpen eUpgrade ci = do
  LimitInput plebSoft plebHard <- counter6Pleb ci
  LimitInput premSoft premHard <- counter6Prem ci
  eSoft <- switchE . leftmost $ [plebSoft <$ eOpen, premSoft <$ eUpgrade]
  eHard <- switchE . leftmost $ [plebHard <$ eOpen, premHard <$ eUpgrade]
  return $
    LimitInput eSoft eHard

myLogicalNetwork :: MonadMoment m => Inputs -> m Outputs
myLogicalNetwork (Inputs eMessage eUpgrade eQuit eUnknownCommand) = do
  UpgradeOutput bAccount <- upgrade $ UpgradeInput eUpgrade
  LimitOutput ecWrite ecQuit <- fmap handleLimit . counter5 eUpgrade $ CounterInput eMessage bAccount
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
