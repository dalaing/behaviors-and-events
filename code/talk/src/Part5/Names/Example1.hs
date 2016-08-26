{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Part5.Names.Example1 (
    go_5_n_1
  ) where

import           Control.Monad               (forever)

import qualified Data.Set                    as S

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Part5.Names.Example1.Cmd
import           Part5.Names.Example1.Common
import           Part5.Names.Example1.Name

data Phase =
    PreOpen
  | NamePrompting
  | CommandProcessing
  deriving (Eq, Ord, Show)

data PhaseInput = PhaseInput {
    pieStartName :: Event ()
  , pieStartCmd  :: Event ()
  }

data PhaseOutput = PhaseOutput {
    pobPhase :: Behavior Phase
  }

handlePhase :: MonadMoment m => PhaseInput -> m PhaseOutput
handlePhase (PhaseInput eName eCmd) = do
  bPhase <- stepper PreOpen . leftmost $ [
      NamePrompting <$ eName
    , CommandProcessing <$ eCmd
    ]
  return $ PhaseOutput bPhase

pureNetworkDescription :: InputIO -> MomentIO OutputIO
pureNetworkDescription (InputIO eOpen eRead) = do

  let
    bNames = pure (S.fromList ["root"])

  rec
    PhaseOutput bPhase <- handlePhase $ PhaseInput eOpen enStop

    let
      eReadName = whenE ((== NamePrompting) <$> bPhase) eRead
      enStop = () <$ eName
    NameOutput enWrite eName <- handleName $ NameInput eOpen eReadName bNames

  bName <- stepper "" eName

  let
    eReadCmd  = whenE ((== CommandProcessing) <$> bPhase) eRead
  CmdOutput ecWrite ecQuit <- handleCmd $ CmdInput enStop eReadCmd bNames bName

  let
    eWrite = leftmost [enWrite, ecWrite]

  return $ OutputIO eWrite ecQuit

duplicateLater :: Event a -> MomentIO (Event a)
duplicateLater e = do
  (eLater, f) <- newEvent
  reactimate $ f <$> e
  return eLater

pureNetworkDescription2 :: InputIO -> MomentIO OutputIO
pureNetworkDescription2 (InputIO eOpen eRead) = mdo

  let
    bNames = pure (S.fromList ["root"])

  -- misses the initial open events if we
  -- - use the open events for the component and the switch
  --   don't use a delay

  eOpenName <- duplicateLater eOpen
  NameOutput enWrite eName <- handleName $ NameInput eOpenName eRead bNames

  bName <- stepper "" esName
  -- TODO
  -- stop outputting bName, output eNameValid, strip to get eOpenCmd, stepper to get bName
  --   see how that goes
  --   - result: no effect, results are still flowing through the network
  --
  -- look at switching inputs as well as outputs
  -- - switch eName is apparently the thing, according to the sodium book
  --   - would be nice to work out the exact circumstances under which things are GCed
  --     - can we add a reactimate inside of NameInput without stopping GC? if so we can demonstrate some things
  --
  -- switching (a -> m b) and then using it might get then done in one go
  -- - feeding the outputs of one stage into the inputs of the next stage might get tricky
  --
  -- probably time to consult the sodium book :)
  let
    eStop = () <$ esName

  eOpenCmd <- duplicateLater eStop
  CmdOutput ecWrite ecQuit <- handleCmd $ CmdInput eOpenCmd eRead bNames bName

  let
    emptyOut = OutputIO never never
    nameOut = OutputIO enWrite never
    cmdOut = OutputIO ecWrite ecQuit

  -- this fails: the whole network is sticking around
    -- can work that out by typing valid names in the cmd phase, we keep getting start signals
    -- might work better if we build the behavior from outside handleName
  esName <- switchE never . leftmost $ [
      eName <$ eOpen
    , never <$ eStop
    ]

  switch emptyOut . leftmost $ [
      nameOut <$ eOpen
    , cmdOut <$ eStop
    ]

data CommonOutput = CommonOutput {
    oeWrite  :: Event String
  , oeQuit   :: Event ()
  , oeName :: Event String
  }

emptyCommonOutput :: CommonOutput
emptyCommonOutput =
  CommonOutput never never never

instance Switch CommonOutput where
  switch e ee =
    CommonOutput <$>
      switchAp oeWrite e ee <*>
      switchAp oeQuit e ee <*>
      switchAp oeName e ee

nameToCommonOutput :: NameOutput -> CommonOutput
nameToCommonOutput (NameOutput eWrite eName) =
  CommonOutput eWrite never eName

cmdToCommonOutput :: CmdOutput -> CommonOutput
cmdToCommonOutput (CmdOutput eWrite eQuit) =
  CommonOutput eWrite eQuit never

altSwitchE :: MonadMoment m => Event a -> Event (Event a) -> m (Event a)
altSwitchE e ee = do
  res <- accumE e (const <$> ee)
  switchE never res

pureNetworkDescription2a :: InputIO -> MomentIO OutputIO
pureNetworkDescription2a (InputIO eOpen eRead) = mdo

  let
    bNames = pure (S.fromList ["root"])

  -- misses the initial open events if we
  -- - use the open events for the component and the switch
  --   don't use a delay

  -- CommonOutput eWrite eQuit eName <- 
    (nameBlock :: Moment CommonOutput) = fmap nameToCommonOutput . handleName $ NameInput eOpenName eRead bNames
    (cmdBlock :: Moment CommonOutput) = fmap cmdToCommonOutput . handleCmd $ CmdInput eOpenCmd eRead bNames bName

  eOpenName <- duplicateLater eOpen
  eOpenCmd <- duplicateLater $ () <$ eName

  let
    block = observeE . leftmost $ [
        nameBlock <$ eOpen
      , cmdBlock <$ eName
      ]

  CommonOutput eWrite eQuit eName <- switch emptyCommonOutput block

  reactimate $ (\n -> putStrLn $ "name " ++ n) <$> eName

  bName <- stepper "" eName

  eTest <- switchE ("A" <$ eRead) $ ("B" <$ eRead) <$ eOpenCmd

  reactimate $ putStrLn <$> eTest

  return $ OutputIO eWrite eQuit

pureNetworkDescription3 :: InputIO -> MomentIO OutputIO
pureNetworkDescription3 (InputIO eOpen eRead) = mdo

  let
    bNames = pure (S.fromList ["root"])

  -- misses the initial open events if we
  -- - use the open events for the component and the switch
  --   don't use a delay

  let
    (eNameStart, nameMoment) = handleName2 $ NameInput eOpen eRead bNames
  NameOutput enWrite eName <- nameMoment

  bName <- stepper "" esName

  let
    enStop = () <$ esName
    (eCmdStart, cmdMoment) = handleCmd2 $ CmdInput enStop eRead bNames bName
  CmdOutput ecWrite ecQuit <- cmdMoment

  esName <- switchE never . leftmost $ [
      eName <$ eOpen
    , never <$ enStop
    ]

  eSwitchWrite <- switchE never . leftmost $ [
      enWrite <$ eOpen
    , ecWrite <$ enStop
    ]

  eQuit <- switchE never . leftmost $ [
      never  <$ eOpen
    , ecQuit <$ enStop
    ]

  let
    eWrite = leftmost [
         eNameStart
      ,  eCmdStart
      ,  eSwitchWrite
      ]

  return $ OutputIO eWrite ecQuit

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x

go_5_n_1 :: IO ()
go_5_n_1 = do
  input <- mkInputSources
  network <- compile $ mkNetwork pureNetworkDescription2a input
  actuate network
  eventLoop input
