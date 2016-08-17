{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part1.Example4 (
    go_1_4
  , testNetwork
  , myNetwork2
  , InputCmd(..)
  , OutputCmd(..)
  , example1Output
  ) where

import           Control.Monad              (forever)
import           Data.Foldable              (traverse_)
import           System.Exit                (exitSuccess)

import           Data.Profunctor

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
  --  o <- fn . fanInput $ i
  --  return $ mergeOutput o

--

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

    commands =
      ["help", "quit"]
    [eHelp, eQuit] =
      fmap (\x -> () <$ filterE (== x) eCommand) commands
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

myLogicalNetwork :: Inputs -> Outputs
myLogicalNetwork (Inputs eOpen eMessage eHelp eQuit eUnknownCommand) =
  let
    eWrites = [
        "Welcome to the echo program!" <$ eOpen
      , eMessage
      , "/help displays this message\n/quit exits the program" <$ eHelp
      , "Bye" <$ eQuit
      , ("Unknown command: " ++) <$> eUnknownCommand
      ]
    eQuits = [
        eQuit
      ]
  in
    Outputs eWrites eQuits

myTestableNetwork :: InputIO -> Moment OutputIO
myTestableNetwork =
  return . fanIn . myLogicalNetwork . fanOut

data OpenInput = OpenInput {
   oieOpen :: Event ()
 }

data OpenOutput = OpenOutput {
   ooeWrite :: Event String
 }

handleOpen :: OpenInput -> OpenOutput
handleOpen (OpenInput eOpen) =
  let
    eWrite = "Welcome to the echo program!" <$ eOpen
  in
    OpenOutput eWrite

data MessageInput = MessageInput {
    mieRead :: Event String
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  }

handleMessage :: MessageInput -> MessageOutput
handleMessage (MessageInput eMessage) =
  MessageOutput eMessage

data HelpInput = HelpInput {
   hieHelp :: Event ()
 }

data HelpOutput = HelpOutput {
   hoeWrite :: Event String
 }

handleHelp :: HelpInput -> HelpOutput
handleHelp (HelpInput eHelp) =
  let
    eWrite = "/help displays this message\n/quit exits the program" <$ eHelp
  in
    HelpOutput eWrite


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
myLogicalNetwork2 (Inputs eOpen eMessage eHelp eQuit eUnknownCommand) =
  let
    OpenOutput eoWrite = handleOpen $ OpenInput eOpen
    MessageOutput emWrite = handleMessage $ MessageInput eMessage
    HelpOutput ehWrite = handleHelp $ HelpInput eHelp
    QuitOutput eqWrite eqQuit = handleQuit $ QuitInput eQuit
    UnknownCommandOutput eucWrite = handleUnknownCommand $ UnknownCommandInput eUnknownCommand
  in
    Outputs [eoWrite, emWrite, ehWrite, eqWrite, eucWrite] [eqQuit]

myNetwork2 :: InputIO -> Moment OutputIO
myNetwork2 =
  return . fanIn . myLogicalNetwork2 . fanOut

example1Input :: [Maybe InputCmd]
example1Input = [Just (Read "one"), Nothing, Just (Read "two"), Just (Read "/quit")]

example1Output :: IO [Maybe [OutputCmd]]
example1Output = testNetwork myNetwork2 example1Input

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

go_1_4 :: IO ()
go_1_4 = do
  input <- mkInputSources
  network <- compile $ mkNetwork myNetwork2 input
  actuate network
  eventLoop input
