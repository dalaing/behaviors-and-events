{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Part3.Example8 where
  -- go_3_8
  -- ) where

import Reactive.Banana
import Control.Lens

import Part3.Common

-- The limit component

data LimitInput = LimitInput {
    lieLimitUp :: Event ()
  , lieLimitDown :: Event ()
  }

data LimitOutput = LimitOutput {
    loeLimit :: Event Int
  , lobLimit :: Behavior Int
  }

handleLimit :: LimitInput -> Moment LimitOutput
handleLimit (LimitInput eUp eDown) = do
  (eLimit, bLimit) <- mapAccum 1 . fmap (\f x -> (f x, f x)) . unions $ [
      (+ 1) <$ eUp
    , (max 0 . subtract 1) <$ eDown
    ]
  return $ LimitOutput eLimit bLimit

-- The message component

data MessageInput = MessageInput {
    mieMessage :: Event String
  , mieLimit :: Event Int
  , mibLimit :: Behavior Int
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  }

addMessage :: Int -> String -> [String] -> [String]
addMessage n m ms =
  take n (m : ms)

handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage eLimit bLimit) = do
  bMessage <- accumB [] . unions $ [
      take <$> eLimit
    , addMessage <$> bLimit <@> eMessage
    ]
  let
    f l h m = m ++ " (last " ++ show l ++ "message: " ++ show h ++ ")"
    eWrite = f <$> bLimit <*> bMessage <@> eMessage
  return $ MessageOutput eWrite

-- The event network

domainNetworkDescription :: Inputs -> Moment Outputs
domainNetworkDescription (Inputs eOpen eMessage eLimitUp eLimitDown eHelp eQuit eUnknown) = do
  OpenOutput eoWrite        <- handleOpen $ OpenInput eOpen
  LimitOutput eLimit bLimit <- handleLimit $ LimitInput eLimitUp eLimitDown
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage eLimit bLimit
  HelpOutput ehWrite        <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown
  return $ Outputs [eoWrite, emWrite, ehWrite, eqWrite, euWrite] [eqQuit]

go_3_8 :: IO ()
go_3_8 =
   mkGo . mkNetwork . liftDomainNetwork $ domainNetworkDescription

-- Testing support for the limit component

{-
instance Fannable LimitInput where
  type ToFan LimitInput = LimitInputCmd
  fanInput eIn =
    let
      eLimitUp   = filterPrism _LLimitUp eIn
      eLimitDown = filterPrism _LLimitDown eIn
    in
      return $ LimitInput eLimitUp eLimitDown

instance Mergable LimitOutput where
  type Merged LimitOutput = LimitOutputCmd
  mergeOutput eSample (LimitOutput eLimit bLimit) =
    unionWith combineLimit
      ((\b e -> LimitOutputCmd b [LLimit e]) <$> bLimit <@> eLimit)
      ((\b -> LimitOutputCmd b []) <$> bLimit <@ eSample)
-}

data LimitInputCmd =
    LLimitUp
  | LLimitDown
  deriving (Eq, Ord, Show)

makePrisms ''LimitInputCmd

instance Fannable () LimitInputCmd where
  type Fanned () LimitInputCmd = LimitInput
  fanInput eIn =
    LimitInput <$>
      fanE _LLimitUp eIn <*>
      fanE _LLimitDown eIn

data LimitOutputEvent =
    LLimit Int
  deriving (Eq, Ord, Show)

makePrisms ''LimitOutputEvent

data LimitOutputCmd = LimitOutputCmd {
    locbLimit :: Int
  , loceLimit :: [LimitOutputEvent]
  } deriving (Eq, Ord, Show)

makeLenses ''LimitOutputCmd

instance Mergable Int LimitOutputEvent where
  type ToMerge Int LimitOutputEvent = LimitOutput
  mergeOutput eSample (LimitOutput eLimit bLimit) =
    unionWith combineResult
      (mergeE bLimit _LLimit eLimit)
      (mergeB bLimit eSample)

-- Testing support the message component

data MessageInputEvent =
    MMessage String
  | MLimit Int
  deriving (Eq, Ord, Show)

makePrisms ''MessageInputEvent

data MessageInputCmd = MessageInputCmd {
    _micbLimit :: Int
  , _micEvent :: MessageInputEvent
  } deriving (Eq, Ord, Show)

makeLenses ''MessageInputCmd

instance Fannable Int MessageInputEvent where
  type Fanned Int MessageInputEvent = MessageInput
  fanInput eIn =
    MessageInput <$>
      fanE _MMessage eIn <*>
      fanE _MLimit eIn <*>
      fanB 0 id eIn

data MessageOutputCmd =
  MWrite String
  deriving (Eq, Ord, Show)

makePrisms ''MessageOutputCmd

instance Mergable () MessageOutputCmd where
  type ToMerge () MessageOutputCmd = MessageOutput
  mergeOutput _ (MessageOutput eWrite) =
    mergeE (pure ()) _MWrite eWrite

-- Testing support for the combination of the limit and message components

data CombinedInput = CombinedInput {
    cieLimitUp :: Event ()
  , cieLimitDown :: Event ()
  , cieLimitMessage :: Event String
  }

data CombinedOutput = CombinedOutput {
    coeWrite :: Event String
  }

combined :: CombinedInput -> Moment CombinedOutput
combined (CombinedInput eLimitUp eLimitDown eMessage) = do
  LimitOutput eLimit bLimit <- handleLimit $ LimitInput eLimitUp eLimitDown
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage eLimit bLimit
  return $ CombinedOutput emWrite

data CombinedInputCmd =
    CLimitUp
  | CLimitDown
  | CMessage String
  deriving (Eq, Ord, Show)

makePrisms ''CombinedInputCmd

instance Fannable () CombinedInputCmd where
  type Fanned () CombinedInputCmd = CombinedInput
  fanInput eIn =
    CombinedInput <$>
      fanE _CLimitUp eIn <*>
      fanE _CLimitDown eIn <*>
      fanE _CMessage eIn

data CombinedOutputCmd =
  CWrite String
  deriving (Eq, Ord, Show)

makePrisms ''CombinedOutputCmd

instance Mergable () CombinedOutputCmd where
  type ToMerge () CombinedOutputCmd = CombinedOutput
  mergeOutput _ (CombinedOutput eWrite) =
    mergeE (pure ()) _CWrite eWrite
