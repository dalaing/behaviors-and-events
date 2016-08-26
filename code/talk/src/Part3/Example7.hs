{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Example7 (
  go_3_7
  ) where

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

addMessage :: Int -> String -> [String] -> [String]
addMessage n m ms =
  take n (m : ms)

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
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
