{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Account.Common (
    mkGo
  , mkTestFn
  , Inputs(..)
  , Outputs(..)
  , OpenInput(..)
  , OpenOutput(..)
  , handleOpen
  , MessageInput(..)
  , MessageOutput(..)
  , handleMessage
  , AccountType(..)
  , UpgradeInput(..)
  , UpgradeOutput(..)
  , handleUpgrade
  , LimitEvents(..)
  , LimitOutput(..)
  , translateLimitEvents
  , HelpInput(..)
  , HelpOutput(..)
  , handleHelp
  , QuitInput(..)
  , QuitOutput(..)
  , handleQuit
  , UnknownInput(..)
  , UnknownOutput(..)
  , handleUnknown
  , leftmost
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

handleInput :: InputSources -> MomentIO InputIO
handleInput (InputSources iso isr) = do
  eOpen <- fromAddHandler . addHandler $ iso
  eRead <- fromAddHandler . addHandler $ isr
  return $ InputIO eOpen eRead

data OutputIO = OutputIO {
    ioeWrite :: Event String
  , ioeClose :: Event ()
  }

handleOutput :: OutputIO -> MomentIO ()
handleOutput (OutputIO eWrite eClose) = do
  reactimate $ putStrLn <$> eWrite
  reactimate $ exitSuccess <$ eClose

mkNetwork :: (InputIO -> Moment OutputIO) -> InputSources -> MomentIO ()
mkNetwork fn input = do
  i <- handleInput input
  o <- liftMoment $ fn i
  handleOutput o

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

data OutputCmd =
    Write String
  | Close
  deriving (Eq, Ord, Show)

mergeOutput :: OutputIO -> Event [OutputCmd]
mergeOutput (OutputIO eWrite eClose) =
  unionWith (++)
    ((\x -> [Write x]) <$> eWrite)
    ([Close] <$ eClose)

testNetwork :: (InputIO -> Moment OutputIO) -> [Maybe InputCmd] -> IO [Maybe [OutputCmd]]
testNetwork fn =
  interpret $ \i -> do
    o <- fn . fanInput $ i
    return $ mergeOutput o

mkTestFn :: (Inputs -> Moment Outputs) -> [Maybe InputCmd] -> IO [Maybe [OutputCmd]]
mkTestFn n =
  let
    pureNetworkDescription i = do
      o <- n . fanOut $ i
      return $ fanIn o
  in
    testNetwork pureNetworkDescription

data Inputs = Inputs {
    ieOpen           :: Event ()
  , ieMessage        :: Event String
  , ieUpgrade        :: Event ()
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

    eUpgrade   = () <$ filterE (== "upgrade") eCommand
    eHelp      = () <$ filterE (== "help") eCommand
    eQuit      = () <$ filterE (== "quit") eCommand

    commands =
      ["upgrade", "help", "quit"]
    eUnknownCommand =
      filterE (`notElem` commands) eCommand
  in
    Inputs eOpen eMessage eUpgrade eHelp eQuit eUnknownCommand

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

data OpenInput  = OpenInput  { oieOpen :: Event () }
data OpenOutput = OpenOutput { ooeWrite :: Event String }

handleOpen :: MonadMoment m => OpenInput -> m OpenOutput
handleOpen (OpenInput eOpen) =
  let
    eWrite = "Hi (type /help for instructions)" <$ eOpen
  in
    return $ OpenOutput eWrite

data MessageInput  = MessageInput  { mieRead  :: Event String }
data MessageOutput = MessageOutput { moeWrite :: Event String }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  return $ MessageOutput eMessage

data AccountType =
    Plebian
  | Premium
  deriving (Eq, Ord, Show)

data UpgradeInput  = UpgradeInput  { uieUpgrade :: Event () }
data UpgradeOutput = UpgradeOutput { uobLimit :: Behavior AccountType }

handleUpgrade :: MonadMoment m => UpgradeInput -> m UpgradeOutput
handleUpgrade (UpgradeInput eUpgrade) = do
  bAccount <- stepper Plebian (Premium <$ eUpgrade)
  return $ UpgradeOutput bAccount

data LimitEvents = LimitEvents {
    lieSoftLimit :: Event ()
  , lieHardLimit :: Event ()
  }

data LimitOutput = LimitOutput {
    loeWrite :: Event String
  , loeQuit  :: Event ()
  }

translateLimitEvents :: LimitEvents -> LimitOutput
translateLimitEvents (LimitEvents eSoftLimit eHardLimit) =
  let
    eSoftLimitMessage =
      "You are using a Plebian account.  Consider upgrading to a Premium account for unlimited messages." <$ eSoftLimit
    eHardLimitMessage =
      "You have reached your message limit for a Plebian account, please upgrade." <$ eHardLimit
    eMessage =
      leftmost [
          eHardLimitMessage
        , eSoftLimitMessage
        ]
    eQuit =
      eHardLimit
  in
    LimitOutput eMessage eQuit

data HelpInput  = HelpInput  { hieHelp :: Event () }
data HelpOutput = HelpOutput { hoeWrite :: Event String }

handleHelp :: MonadMoment m => HelpInput -> m HelpOutput
handleHelp (HelpInput eHelp) =
  let
    helpStrings = [
        "/upgrade upgrade to a premium account"
      , "/help displays this message"
      , "/quit exits the program"
      ]
    eWrite = unlines helpStrings <$ eHelp
  in
    return $ HelpOutput eWrite

data QuitInput = QuitInput {
    qieQuit :: Event ()
  }

data QuitOutput = QuitOutput {
    qoeWrite :: Event String
  , qoeQuit  :: Event ()
  }

handleQuit :: MonadMoment m => QuitInput -> m QuitOutput
handleQuit (QuitInput eQuit) =
  let
    eWrite = "Bye" <$ eQuit
  in
    return $ QuitOutput eWrite eQuit

data UnknownInput  = UnknownInput  { ucieCommand :: Event String }
data UnknownOutput = UnknownOutput { ucoeWrite :: Event String }

handleUnknown :: MonadMoment m => UnknownInput -> m UnknownOutput
handleUnknown (UnknownInput eUnknown) =
  let
      msg x = "Unknown command: " ++ x ++ " (type /help for instructions)"
  in
    return . UnknownOutput $ msg <$> eUnknown

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

mkGo :: (Inputs -> Moment Outputs) -> IO ()
mkGo n = do
  input <- mkInputSources
  let
    networkDescription = mkNetwork $ \i -> do
      o <- n . fanOut $ i
      return $ fanIn o
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
