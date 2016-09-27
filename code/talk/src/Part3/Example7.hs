{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Part3.Example7 where
  -- go_3_8
  -- ) where

import           Control.Lens
import           Reactive.Banana

import           Part3.Common

-- The limit component

data LimitInput = LimitInput {
    lieLimitUp   :: Event ()
  , lieLimitDown :: Event ()
  }

data LimitOutput = LimitOutput {
    loeLimit :: Event Int
  , lobLimit :: Behavior Int
  }

handleLimit :: LimitInput -> Moment LimitOutput
handleLimit (LimitInput eUp eDown) = do
  (eLimit, bLimit) <- mapAccum 1 . fmap (\f x -> (f x, f x)) . unions $ [
      succ <$ eUp
    , (max 0 . pred) <$ eDown
    ]
  return $ LimitOutput eLimit bLimit

-- The message component

data MessageInput = MessageInput {
    mieMessage :: Event String
  , mieLimit   :: Event Int
  , mibLimit   :: Behavior Int
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  }

handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage eLimit bLimit) = do
  bMessages <- accumB [] $ unions [
      (\n x xs -> take n (x : xs)) <$> bLimit <@> eMessage
    , take <$> eLimit
    ]

  let
    format n ls m = m ++ " (" ++ show n ++ " previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessages eMessage
    eOut = leftmost [
        format <$> bLimit <*> bMessages <@> eMessageWithHistory
      , eMessage
      ]

  return $ MessageOutput eOut

-- The event network

networkDescription :: Inputs -> Moment Outputs
networkDescription (Inputs eOpen (ReadInputs eMessage eLimitUp eLimitDown eHelp eUnknown eQuit)) = do
  OpenOutput eoWrite        <- handleOpen $ OpenInput eOpen
  LimitOutput eLimit bLimit <- handleLimit $ LimitInput eLimitUp eLimitDown
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage eLimit bLimit
  HelpOutput ehWrite        <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown
  return $ Outputs (WriteOutputs eoWrite emWrite ehWrite euWrite eqWrite) eqQuit


go_3_7 :: IO ()
go_3_7 =
  mkGo . mkNetwork . handleIO $ networkDescription

-- Testing support for the limit component

data LimitInputE =
    LLimitUp
  | LLimitDown
  deriving (Eq, Ord, Show)

makePrisms ''LimitInputE

instance Fannable () LimitInputE where
  type Fanned () LimitInputE = LimitInput
  fanInput eIn =
    LimitInput <$>
      fanE _LLimitUp eIn <*>
      fanE _LLimitDown eIn
  fanInput2 _ eIn =
    LimitInput <$>
      return (filterPrism _LLimitUp eIn) <*>
      return (filterPrism _LLimitDown eIn)

instance Mergable Int Int where
  type ToMerge Int Int = LimitOutput
  mergeOutput eSample (LimitOutput eLimit bLimit) =
    unionWith combineResult
      (mergeE bLimit id eLimit)
      (mergeB bLimit eSample)

testLimit :: [Maybe (Command () LimitInputE)] -> IO [Maybe (Result Int Int)]
testLimit = testNetwork handleLimit

lTestMe2 :: IO [Maybe (Result Int Int)]
lTestMe2 = testNetwork2 handleLimit () . fmap Just $ lcmds

lcmds = [
    Command () LLimitUp
  , Command () LLimitUp
  , Command () LLimitUp
  , Command () LLimitDown
  , Command () LLimitUp
  , Command () LLimitDown
  , Command () LLimitUp
  , Command () LLimitUp
  , Command () LLimitDown
  , Command () LLimitUp
  , Command () LLimitUp
  , Command () LLimitDown
  ]
-- Testing support the message component

data MessageInputE =
    MMessage String
  | MLimit Int
  deriving (Eq, Ord, Show)

makePrisms ''MessageInputE

instance Fannable Int MessageInputE where
  type Fanned Int MessageInputE = MessageInput
  fanInput eIn =
    MessageInput <$>
      fanE _MMessage eIn <*>
      fanE _MLimit eIn <*>
      fanB 0 id eIn
  fanInput2 b eIn =
    MessageInput <$>
      return (filterPrism _MMessage eIn) <*>
      return (filterPrism _MLimit eIn) <*>
      return b

data MessageOutputE =
  MWrite String
  deriving (Eq, Ord, Show)

makePrisms ''MessageOutputE

instance Mergable () MessageOutputE where
  type ToMerge () MessageOutputE = MessageOutput
  mergeOutput _ (MessageOutput eWrite) =
    mergeE (pure ()) _MWrite eWrite

testMessage :: [Maybe (Command Int MessageInputE)] -> IO [Maybe (Result () MessageOutputE)]
testMessage = testNetwork handleMessage

testMe = testMessage . fmap Just $ cmds
testMe2 :: IO [Maybe (Result () MessageOutputE)]
testMe2 = testNetwork2 handleMessage 1 . fmap Just $ cmds

cmds = [
    Command 1 (MMessage "a")
  , Command 1 (MMessage "b")
  , Command 1 (MMessage "c")
  , Command 1 (MLimit 2)
  , Command 2 (MMessage "d")
  , Command 2 (MMessage "e")
  , Command 2 (MMessage "f")
  , Command 2 (MLimit 3)
  , Command 3 (MMessage "g")
  , Command 3 (MMessage "h")
  , Command 3 (MMessage "i")
  , Command 3 (MLimit 2)
  , Command 2 (MMessage "j")
  , Command 2 (MMessage "k")
  , Command 2 (MMessage "l")
  ]

-- Testing support for the combination of the limit and message components

data CombinedInput = CombinedInput {
    cieLimitUp      :: Event ()
  , cieLimitDown    :: Event ()
  , cieLimitMessage :: Event String
  }

data CombinedOutput = CombinedOutput {
    coeWrite :: Event String
  }

combined :: CombinedInput -> Moment CombinedOutput
combined (CombinedInput eLimitUp eLimitDown eMessage) = do
  LimitOutput eLimit bLimit  <- handleLimit $ LimitInput eLimitUp eLimitDown
  MessageOutput emWrite <- handleMessage $ MessageInput eMessage eLimit bLimit
  return $ CombinedOutput emWrite

data CombinedInputE =
    CLimitUp
  | CLimitDown
  | CMessage String
  deriving (Eq, Ord, Show)

makePrisms ''CombinedInputE

instance Fannable () CombinedInputE where
  type Fanned () CombinedInputE = CombinedInput
  fanInput eIn =
    CombinedInput <$>
      fanE _CLimitUp eIn <*>
      fanE _CLimitDown eIn <*>
      fanE _CMessage eIn

data CombinedOutputE =
  CWrite String
  deriving (Eq, Ord, Show)

makePrisms ''CombinedOutputE

instance Mergable () CombinedOutputE where
  type ToMerge () CombinedOutputE = CombinedOutput
  mergeOutput _ (CombinedOutput eWrite) =
    mergeE (pure ()) _CWrite eWrite
