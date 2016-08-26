{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Part3.Mem (
    go
  ) where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever, when)
import           System.Mem

import           Reactive.Banana
import           Reactive.Banana.Frameworks

orElse :: Event a -> Event a -> Event a
orElse = unionWith const

leftmost :: [Event a] -> Event a
leftmost = foldl orElse never

duplicateLater :: Event a -> MomentIO (Event a)
duplicateLater e = do
  (eLater, f) <- newEvent
  reactimate $ f <$> e
  return eLater

data AccumType = ATEvent | ATBehavior

data BlockConfig = BlockConfig {
    bcName            :: String
  , bcTicksPerAccum   :: Int
  , bcAccumsPerSwitch :: Int
  , bcAccumSize       :: Int
  , bcAccumType       :: AccumType
  , bcPrintInternal   :: Bool
  }

data ProgramType =
    FilterInputs
  | SwitchOutput
  | SwitchOutputs
  | SwitchInput
  | SwitchInputAndOutput
  | SwitchInputAndOutputs

data ProgramConfig = ProgramConfig {
    pcType          :: ProgramType
  , pcAccumSize     :: Int
  , pcAccumType     :: AccumType
  , pcPrintInternal :: Bool
  }

mkBlockA :: ProgramConfig -> BlockConfig
mkBlockA (ProgramConfig _ pcS pcT pcP) =
  BlockConfig "A" 3 4 pcS pcT pcP

mkBlockB :: ProgramConfig -> BlockConfig
mkBlockB (ProgramConfig _ pcS pcT pcP) =
  BlockConfig "B" 5 4 pcS pcT pcP

data BlockInput = BlockInput {
    bieTick :: Event ()
  }

data BlockOutput = BlockOutput {
    boeSwitch :: Event ()
  , boeOut    :: Event [Int]
  }

mkBlock :: BlockConfig -> BlockInput -> MomentIO BlockOutput
mkBlock bc (BlockInput eTick) = do
  -- will this fire every switch? or just the first one?
  -- if it fires every time, note that somewhere, then do a major gc here
  _ <- when (bcPrintInternal bc) $
    liftIOLater . putStrLn $ bcName bc ++ " started"

  eTicks <- accumE 0 ((+ 1) <$ eTick)

  _ <- when (bcPrintInternal bc) $
    reactimate $ (\x -> putStrLn $ bcName bc ++ " " ++ show x) <$> eTicks

  let
    tpa = bcTicksPerAccum bc
    eAccum = filterE (\t -> t `mod` tpa == tpa - 1) eTicks

  eAccums <- accumE 0 ((+ 1) <$ eAccum)

  let
    eSwitch = () <$ filterE (\a -> a == bcAccumsPerSwitch bc) eAccums

  eOut <- case bcAccumType bc of
    ATEvent ->
      accumE [] ((\x -> enumFromTo 0 (bcAccumSize bc) ++ x) <$ eAccum)
    ATBehavior -> do
      b <- accumB [] ((\x -> enumFromTo 0 (bcAccumSize bc) ++ x) <$ eAccum)
      return $ b <@ eTick
  return $ BlockOutput eSwitch eOut

{-
data ProgramType =
    FilterInputs
  | SwitchOutput
  | SwitchOutputs
  | SwitchInput
  | SwitchInputAndOutput
  | SwitchInputAndOutputs
-}

data InputEvents = InputEvents {
    ieStart :: Event ()
  , ieTick  :: Event ()
  }

mkProgram :: ProgramConfig -> InputEvents -> MomentIO ()
mkProgram pc = case pcType pc of
  FilterInputs ->
    mkProgramFilterInputs pc
  SwitchOutput ->
    mkProgramSwitchOutput pc
  SwitchOutputs ->
    mkProgramSwitchOutputs pc
  SwitchInput ->
    mkProgramSwitchInput pc
  SwitchInputAndOutput ->
    mkProgramSwitchInputAndOutput pc
  SwitchInputAndOutputs ->
    mkProgramSwitchInputAndOutputs pc

data Selection =
    A
  | B
  deriving (Eq, Ord, Show)

mkProgramFilterInputs :: ProgramConfig -> InputEvents -> MomentIO ()
mkProgramFilterInputs pc (InputEvents eStart eTick) = mdo

  BlockOutput eSwitchA eOutA <- mkBlock (mkBlockA pc) (BlockInput eTickA)
  BlockOutput eSwitchB eOutB <- mkBlock (mkBlockB pc) (BlockInput eTickB)

  reactimate $ (\x -> putStrLn $ "A length: " ++ show x) <$> eOutA
  reactimate $ (\x -> putStrLn $ "B length: " ++ show x) <$> eOutB

  bSelection <- stepper A . leftmost $ [
      B <$ eSwitchA
    , A <$ eSwitchB
    ]

  let
    eTickA = whenE ((== A) <$> bSelection) eTick
    eTickB = whenE ((== B) <$> bSelection) eTick

  return ()

mkProgramSwitchOutput :: ProgramConfig -> InputEvents -> MomentIO ()
mkProgramSwitchOutput pc (InputEvents eStart eTick) = do

  BlockOutput eSwitchA eOutA <- mkBlock (mkBlockA pc) (BlockInput eTick)
  BlockOutput eSwitchB eOutB <- mkBlock (mkBlockB pc) (BlockInput eTick)

  return ()

mkProgramSwitchOutputs :: ProgramConfig -> InputEvents -> MomentIO ()
mkProgramSwitchOutputs pc (InputEvents eStart eTick) = do

  BlockOutput eSwitchA eOutA <- mkBlock (mkBlockA pc) (BlockInput eTick)
  BlockOutput eSwitchB eOutB <- mkBlock (mkBlockB pc) (BlockInput eTick)

  return ()

mkProgramSwitchInput :: ProgramConfig -> InputEvents -> MomentIO ()
mkProgramSwitchInput pc (InputEvents eStart eTick) = do

  BlockOutput eSwitchA eOutA <- mkBlock (mkBlockA pc) (BlockInput eTick)
  BlockOutput eSwitchB eOutB <- mkBlock (mkBlockB pc) (BlockInput eTick)

  return ()

mkProgramSwitchInputAndOutput :: ProgramConfig -> InputEvents -> MomentIO ()
mkProgramSwitchInputAndOutput pc (InputEvents eStart eTick) = do

  BlockOutput eSwitchA eOutA <- mkBlock (mkBlockA pc) (BlockInput eTick)
  BlockOutput eSwitchB eOutB <- mkBlock (mkBlockB pc) (BlockInput eTick)

  return ()

mkProgramSwitchInputAndOutputs :: ProgramConfig -> InputEvents -> MomentIO ()
mkProgramSwitchInputAndOutputs pc (InputEvents eStart eTick) = do

  BlockOutput eSwitchA eOutA <- mkBlock (mkBlockA pc) (BlockInput eTick)
  BlockOutput eSwitchB eOutB <- mkBlock (mkBlockB pc) (BlockInput eTick)

  return ()

mkCrappyBlock :: String -> Int -> Event () -> MomentIO (Event (String, [Int]))
--mkCrappyBlock :: String -> Event () -> Moment (Event (String, [Int]))
mkCrappyBlock name size eTick = do
  eData <- accumE [] ((enumFromTo 0 size ++) <$ eTick)
  -- reactimate $ putStrLn name <$ eTick
  -- reactimate $ (\x -> putStrLn $ name ++ " inner " ++ show (length x)) <$> eData
  return $ (\x -> (name, x)) <$> eData

mkCrappyProgram1 :: InputEvents -> MomentIO ()
mkCrappyProgram1 (InputEvents eStart eTick) = mdo

  eTicks <- accumE 0 ((+ 1) <$ eTick)

  let
    eSwitchA = () <$ filterE (\x -> x `mod` 10 == 9) eTicks
    eSwitchB = () <$ filterE (\x -> x `mod` 10 == 4) eTicks

  eBlockA <- mkCrappyBlock "A" 1000000 eTick
  eBlockB <- mkCrappyBlock "B" 1000000 eTick

{-
  eInA <- switchE . leftmost $ [
      eTick <$ eStart
    , eTick <$ eSwitchA
    , never <$ eSwitchB
    ]

  eInB <- switchE . leftmost $ [
      never <$ eStart
    , never <$ eSwitchA
    , eTick <$ eSwitchB
    ]
-}

  eOut <- switchE eBlockA . leftmost $ [
      eBlockA <$ eSwitchA
    , eBlockB <$ eSwitchB
    ]

  -- execute and then switch seems to accumulate blocks, at least if reactimate is in there
  -- return ()
  reactimate $ (\(n, x) -> putStrLn $ n ++ " " ++ show (length x)) <$> eOut

mkCrappyProgram2 :: InputEvents -> MomentIO ()
mkCrappyProgram2 (InputEvents eStart eTick) = mdo

  eTicks <- accumE 0 ((+ 1) <$ eTick)

  let
    eSwitchA = () <$ filterE (\x -> x `mod` 10 == 9) eTicks
    eSwitchB = () <$ filterE (\x -> x `mod` 10 == 4) eTicks

  eBlockA <- mkCrappyBlock "A" 1000 eInA
  eBlockB <- mkCrappyBlock "B" 1000 eInB

  eInA <- switchE never . leftmost $ [
      eTick <$ eStart
    , eTick <$ eSwitchA
    , never <$ eSwitchB
    ]

  eInB <- switchE never . leftmost $ [
      never <$ eStart
    , never <$ eSwitchA
    , eTick <$ eSwitchB
    ]

  eOut <- switchE never . leftmost $ [
      eBlockA <$ eStart
    , eBlockA <$ eSwitchA
    , eBlockB <$ eSwitchB
    ]

  reactimate $ (\(n, x) -> putStrLn $ n ++ " " ++ show (length x)) <$> eOut

mkCrappyProgram3 :: InputEvents -> MomentIO ()
mkCrappyProgram3 (InputEvents eStart eTick) = mdo

  eTicks <- accumE 0 ((+ 1) <$ eTick)

  let
    eSwitchA = () <$ filterE (\x -> x `mod` 10 == 9) eTicks
    eSwitchB = () <$ filterE (\x -> x `mod` 10 == 4) eTicks

  let
    eBlockA = mkCrappyBlock "A" 1000000 eTick
    eBlockB = mkCrappyBlock "B" 1000000 eTick

  --let
  --  eBlock = observeE . leftmost $ [
  eBlock <- execute . leftmost $ [
        eBlockA <$ eStart
      , eBlockA <$ eSwitchA
      , eBlockB <$ eSwitchB
      ]

  -- Notes / plan for the telling of this
  -- - build it in, switch, show the memory leak
  -- - build it in, switch and filter the inputs, show the memory leak doesn't get worse
  -- - build it in, combine observe and switch, show the memory leak goes away
  -- - add some IO, show that reactimate will keep things alive

  eOut <- switchE never eBlock

  eClean <- duplicateLater $ () <$ leftmost [eSwitchA, eSwitchB]
  reactimate $ performMajorGC <$ eClean

  -- execute and then switch seems to accumulate blocks, at least if reactimate is in there
  -- return ()
  reactimate $ (\(n, x) -> putStrLn $ n ++ " " ++ show (length x)) <$> eOut

-- crappy block
-- takes a tick, accumulates all ticks it sees and prints it's name each tick
-- output is block name and ticks seen so far, either as an event or as a behavior
-- externally to that, switch between the two every 5 ticks, print the output

-- things to try
  -- example program that accumulates memory predictably based on two different blocks
     -- switch between blocks periodically, look at memory profile
     -- feed in a 1 sec timer
     -- 3 secs for one block, 5 for the other
     -- - capture something big in an event / behavior accumulation, print its length post-switch so that it doesn't get thrown away
     -- 4 blocks worth and then switch
     -- trigger a major gc one delay after switching
     -- optionally print something inside of the blocks, see if reactimate prevents garbage collection
     -- options: just filter input
     --          switch outputs except event that triggers switching
     --          switch inputs except event that triggers switching
     --          switch inputs and outputs except event that triggers switching
     --          switch outputs
     --          switch inputs
     --          switch inputs and outputs

data EventSource a = EventSource {
    addHandler :: AddHandler a
  , fire       :: a -> IO ()
  }

mkEventSource :: IO (EventSource a)
mkEventSource =
  uncurry EventSource <$> newAddHandler

data InputSources = InputSources {
    isOpen :: EventSource ()
  , isTick :: EventSource ()
  }

mkInputSources :: IO InputSources
mkInputSources =
  InputSources <$> mkEventSource <*> mkEventSource

handleInput :: InputSources -> MomentIO InputEvents
handleInput (InputSources iso isr) = do
  eOpen <- fromAddHandler . addHandler $ iso
  eTick <- fromAddHandler . addHandler $ isr
  return $ InputEvents eOpen eTick

eventLoop :: InputSources -> IO ()
eventLoop (InputSources o t) = do
  fire o ()
  -- possibly needs to go in a forkIO, possibly not
  forever $ do
    threadDelay 1000000
    fire t ()

wrapCrappyProgram :: InputSources -> MomentIO ()
wrapCrappyProgram i = do
  e <- handleInput i
  mkCrappyProgram3 e

go :: IO ()
go = do
  input <- mkInputSources
  network <- compile $ wrapCrappyProgram input
  actuate network
  eventLoop input
