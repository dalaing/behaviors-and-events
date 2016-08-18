{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Message.Example5 (
  go_3_m_5
  ) where

import Reactive.Banana

import Part3.Message.Common

data MessageInput = MessageInput {
    mieRead  :: Event String
  , mibLimit :: Behavior Int
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage bLimit) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    f n ms m = m ++ " (previous " ++ show n ++ " messages: " ++ show (take n ms) ++ ")"
    eOut = f <$> bLimit <*> bMessages <@> eMessage
  return $ MessageOutput eOut

domainNetworkDescription :: MonadMoment m => Inputs -> m Outputs
domainNetworkDescription (Inputs eOpen eMessage _ _ eHelp eQuit eUnknown) = do
  OpenOutput eoWrite        <- handleOpen $ OpenInput eOpen
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage (pure 3)
  HelpOutput ehWrite        <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown
  return $ Outputs [eoWrite, emWrite, ehWrite, eqWrite, euWrite] [eqQuit]

go_3_m_5 :: IO ()
go_3_m_5 = mkGo domainNetworkDescription
