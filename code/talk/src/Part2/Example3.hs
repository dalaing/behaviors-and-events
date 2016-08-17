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

import           Data.Profunctor
import qualified Data.Set                   as S

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

data InputIO = InputIO {
    ioeOpen :: Event ()
  , ioeRead :: Event String
  }

data InputCmd =
    Open
  | Read String
  deriving (Eq, Ord, Show)

fanInput :: Event InputCmd -> InputIO
fanInput eIn =
  let
    maybeOpen Open = Just ()
    maybeOpen _    = Nothing
    eOpen = filterJust $ maybeOpen <$> eIn

    maybeRead (Read x) = Just x
    maybeRead _ = Nothing
    eRead = filterJust $ maybeRead <$> eIn
  in
    InputIO eOpen eRead

mergeInput :: InputIO -> Event InputCmd
mergeInput (InputIO eOpen eRead) =
  leftmost [
      Open <$ eOpen
    , Read <$> eRead
    ]

-- have main logic in terms of InputIO -> Moment OutputIO
-- add a test harness so that can be exposed in terms of Event InputCmd -> Moment (Event [OutputCmd])
handleInput :: InputSources -> MomentIO InputIO
handleInput (InputSources iso isr) = do
  eOpen <- fromAddHandler . addHandler $ iso
  eRead <- fromAddHandler . addHandler $ isr
  return $ InputIO eOpen eRead

data OutputIO = OutputIO {
    ioeWrite :: Event String
  , ioeClose :: Event ()
  }

data OutputCmd =
    Write String
  | Close
  deriving (Eq, Ord, Show)

fanOutput :: Event [OutputCmd] -> OutputIO
fanOutput eOut =
  let
    gatherWrite (Write x) = x
    gatherWrite _ = []

    combineWrites = (>>= gatherWrite)

    eWrite = filterE (not . null) $ combineWrites <$> eOut

    eClose = () <$ filterE (Close `elem`) eOut
  in
    OutputIO eWrite eClose

mergeOutput :: OutputIO -> Event [OutputCmd]
mergeOutput (OutputIO eWrite eClose) =
  unionWith (++)
    ((\x -> [Write x]) <$> eWrite)
    ([Close] <$ eClose)

handleOutput :: OutputIO -> MomentIO ()
handleOutput (OutputIO eWrite eClose) = do
  reactimate $ putStrLn <$> eWrite
  reactimate $ exitSuccess <$ eClose

mkNetwork :: (InputIO -> Moment OutputIO) -> InputSources -> MomentIO ()
mkNetwork fn input = do
  i <- handleInput input
  o <- liftMoment $ fn i
  handleOutput o

testNetwork :: (InputIO -> Moment OutputIO) -> [Maybe InputCmd] -> IO [Maybe [OutputCmd]]
testNetwork =
  interpret .
  runStar .
  dimap fanInput mergeOutput .
  Star
  -- interpret $ \i -> do
  --  o <- fn (fanInput i)
  --  return $ mergeOutput o

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
    PreOpen
  | NamePrompting
  | CommandProccessing
  deriving (Eq, Ord, Show)

data PhaseInput = PhaseInput {
    pieOpened     :: Event ()
  , pieNameChosen :: Event ()
  }

data PhaseOutput = PhaseOutput {
    pobPhase :: Behavior Phase
  }

handlePhase :: MonadMoment m => PhaseInput -> m PhaseOutput
handlePhase (PhaseInput eOpen eName) = do
  bPhase <- stepper PreOpen . leftmost $ [
      NamePrompting <$ eOpen
    , CommandProccessing <$ eName
    ]
  return $ PhaseOutput bPhase

data NameInput = NameInput {
    nieIn    :: InputIO
  , nieNames :: Behavior (S.Set String)
  }

data NameError =
    EmptyName
  | MultiWordName String
  | IllegalCharNameError String
  | NameAlreadyInUse String
  deriving (Eq, Ord, Show)

-- we either want eNamePrompt to kick things over (while staying separate from IO)
-- or eWrite, to handle the prompting when things gets started and printing errors messages
-- when error occur
data NameOutput = NameOutput {
    nieNameValid   :: Event String
  , nieNameInvalid :: Event NameError
  , nibName        :: Behavior String
  }

handleName :: NameInput -> Moment NameOutput
handleName (NameInput (InputIO eOpen eRead) bNames) = do
  let
    f names s =
      case words s of
        [] ->
          Left EmptyName
        [x]
          | '/' `elem` x ->
            Left $ IllegalCharNameError s
          | s `S.member` names ->
            Left $ NameAlreadyInUse s
          | otherwise ->
            Right s
        _ ->
          Left $ MultiWordName s
    (eInvalid, eValid) = split $ f <$> bNames <@> eRead
  bName <- stepper "" eValid
  return $ NameOutput eValid eInvalid bName

data Inputs = Inputs {
    ieOpen           :: Event ()
  , ieMessage        :: Event String
  , ieQuit           :: Event ()
  , ieUnknownCommand :: Event String
  }

fanOut :: InputIO -> Inputs
fanOut (InputIO eOpen eRead) =
  let
    eReadNonEmpty =
      filterE (not . null) $ eRead

    isMessage =
      (/= "/") . take 1
    eMessage =
      filterE isMessage eReadNonEmpty

    isCommand =
      (== "/") . take 1
    eCommand =
      fmap (drop 1) . filterE isCommand $ eReadNonEmpty

    commands =
      ["quit"]
    [eQuit] =
      fmap (\x -> () <$ filterE (== x) eCommand) commands
    eUnknownCommand =
      filterE (`notElem` commands) eCommand
  in
    Inputs eOpen eMessage eQuit eUnknownCommand

data Outputs = Outputs {
    oeWrite :: [Event String]
  , oeClose :: [Event ()]
  }

{-
fanIn :: Outputs -> Event [OutputIO]
fanIn (Outputs eWrites eCloses) =
  let
    eCombinedWrites = fmap (\x xs -> Write x : xs) <$> eWrites
    eCombinedCloses = [(Close :) <$ leftmost eCloses]
  in
    fmap ($ []) .
    unions $
    eCombinedWrites ++ eCombinedCloses
-}

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
myLogicalNetwork (Inputs eOpen eMessage eQuit eUnknownCommand) = do
  let
    MessageOutput emWrite = handleMessage $ MessageInput eMessage
    QuitOutput eqWrite eqQuit = handleQuit $ QuitInput eQuit
    UnknownCommandOutput eucWrite = handleUnknownCommand $ UnknownCommandInput eUnknownCommand
  return $ Outputs [emWrite, eqWrite, eucWrite] [eqQuit]

{-
myTestableNetwork :: Event InputCmd -> Moment (Event [OutputCmd])
myTestableNetwork eIn = do
  n <- liftMoment . myLogicalNetwork . fanOut $ eIn
  return $ fanIn n
-}

example1 :: [Maybe InputCmd]
example1 = [Just (Read "one"), Nothing, Just (Read "two"), Just (Read "/quit")]

{-
handleOutput :: OutputIO -> IO ()
handleOutput (Write s) = putStrLn s
handleOutput Close = exitSuccess

networkDescription :: InputSources -> MomentIO ()
networkDescription is = do
  i <- handleInput is
  o <- liftMoment $ myTestableNetwork i
  reactimate $ traverse_ handleOutput <$> o
-}

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

go_2_3 :: IO ()
go_2_3 = do
  inputSources <- mkInputSources
--  network <- compile $ networkDescription inputSources
  network <- compile $ mkNetwork undefined inputSources
  actuate network
  eventLoop inputSources
