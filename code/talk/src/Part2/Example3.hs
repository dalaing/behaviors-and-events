{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part2.Example3 (
    go_2_3
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

data InputSources = InputSources {
    isOpen :: EventSource ()
  , isRead :: EventSource String
  }

mkInputSources :: IO InputSources
mkInputSources =
  InputSources <$> mkEventSource <*> mkEventSource

data InputIO =
    Open
  | Read String
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

handleInput :: InputSources -> MomentIO (Event InputIO)
handleInput (InputSources iso isr) = do
  eOpen <- fromAddHandler . addHandler $ iso
  eRead <- fromAddHandler . addHandler $ isr
  return $ leftmost [
      Open <$ eOpen
    , Read <$> eRead
    ]

  -- TODO some kind of pre-open phase?
  -- if we're using eOpen and eValidName to drive the phases
  -- could have a post-close phase for printing stuff after quit
  -- would be nice to have ePhase to use to alter the network as we go
  --
  -- start with eIn connected to blocks for all of the phases
  -- use bPhase to filter it as appropriate
  -- this introduces value recursion
  --
  -- would be nice to have no Maybe in bName
  -- can start bName off as "" inside the name component
  -- - can even start it outside, holding on eValidName
  -- could possibly do something with sample and pure
  -- this might be a mess until we introduce switch
  --
  -- then head for an approach using switch or something similar
  -- so that we don't do per-event filtering that we don't need to
  -- also mention that it allows for things to be GCed, if a phase that is over
  -- is holding onto memory
  --
  -- how do we pull eValidName out of a homogoneous set of behaviours that we are choosing between?
  -- maybe go for a superset-template of the different phases and plug the holes with `never` - not sure what to do when behaviours are meant to come out of these things - maybe they can't
data Phase =
    NamePrompting
  | CommandProccessing
  deriving (Eq, Ord, Show)

data PhaseInput = PhaseInput {
    pibName :: Behavior (Maybe String)
  }

data PhaseOutput = PhaseOutput {
    pobPhase :: Behavior Phase
  }

handlePhase :: PhaseInput -> PhaseOutput
handlePhase (PhaseInput bName) =
  let
    f Nothing = NamePrompting
    f (Just _) = CommandProccessing
    bPhase = f <$> bName
  in
    PhaseOutput bPhase

data NameInput = NameInput {
    nieIn :: Event InputIO
  }

data NameError =
    EmptyName
  | MultiWordName String
  | IllegalCharNameError String
  deriving (Eq, Ord, Show)

data NameOutput = NameOutput {
    nieNameValid :: Event String
  , nieNameInvalid :: Event NameError
  , nibName :: Behavior (Maybe String)
  }

handleName :: NameInput -> Moment NameOutput
handleName (NameInput eIn) = do
  let
    eRead = inputToRead eIn
    f s =
      case words s of
        [] ->
          Left EmptyName
        [x] ->
          if '/' `elem` x
          then Left $ IllegalCharNameError s
          else Right s
        _ ->
          Left $ MultiWordName s
    (eInvalid, eValid) = split . fmap f $ eRead
  bName <- stepper Nothing . fmap Just $ eValid
  return $ NameOutput eValid eInvalid bName

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

myLogicalNetwork :: Inputs -> Moment Outputs
myLogicalNetwork (Inputs eMessage eUpgrade eQuit eUnknownCommand) = do
  let
    MessageOutput emWrite = handleMessage $ MessageInput eMessage
    QuitOutput eqWrite eqQuit = handleQuit $ QuitInput eQuit
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

networkDescription :: InputSources -> MomentIO ()
networkDescription is = do
  i <- handleInput is
  o <- liftMoment $ myTestableNetwork i
  reactimate $ traverse_ handleOutput <$> o

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

go_2_3 :: IO ()
go_2_3 = do
  inputSources <- mkInputSources
  network <- compile $ networkDescription inputSources
  actuate network
  eventLoop inputSources
