{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
module Part3.Example7 where
  -- go_3_7
  -- ) where

import Reactive.Banana

import Part3.Common

data MessageInput = MessageInput {
    mieMessage :: Event String
  , mieLimit :: Event Int
  , mibLimit :: Behavior Int
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  }

data MessageInputEvent =
    MMessage String
  | MLimit Int
  deriving (Eq, Ord, Show)

data MessageInputCmd = MessageInputCmd {
    micbLimit :: Int
  , micEvent :: MessageInputEvent
  } deriving (Eq, Ord, Show)

instance Fannable MessageInput where
  type ToFan MessageInput = MessageInputCmd
  fanInput eIn = do
    bLimit <- stepper 0 (micbLimit <$> eIn)

    let
      maybeMessage (MMessage x) = Just x
      maybeMessage _           = Nothing
      eMessage = filterJust $ (maybeMessage . micEvent) <$> eIn

      maybeLimit (MLimit x) = Just x
      maybeLimit _         = Nothing
      eLimit = filterJust $ (maybeLimit . micEvent) <$> eIn

    return $ MessageInput eMessage eLimit bLimit

data MessageOutputCmd =
  MWrite String
  deriving (Eq, Ord, Show)

instance Mergable MessageOutput where
  type Merged MessageOutput = MessageOutputCmd
  mergeOutput _ (MessageOutput eWrite) =
    fmap (\x -> [MWrite x]) eWrite

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

domainNetworkDescription :: Inputs -> Moment Outputs
domainNetworkDescription (Inputs eOpen eMessage eLimitUp eLimitDown eHelp eQuit eUnknown) = do
  OpenOutput eoWrite        <- handleOpen $ OpenInput eOpen
  LimitOutput eLimit bLimit <- handleLimit $ LimitInput eLimitUp eLimitDown
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage eLimit bLimit
  HelpOutput ehWrite        <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown
  return $ Outputs [eoWrite, emWrite, ehWrite, eqWrite, euWrite] [eqQuit]

go_3_7 :: IO ()
go_3_7 =
   mkGo . mkNetwork . liftDomainNetwork $ domainNetworkDescription
