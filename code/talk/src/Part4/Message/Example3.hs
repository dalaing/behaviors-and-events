{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Message.Example3 (
  go_3_m_3
  ) where

import Reactive.Banana

import Part3.Message.Common

data MessageInput  = MessageInput  { mieRead  :: Event String }
data MessageOutput = MessageOutput { moeWrite :: Event String }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    f ms m = m ++ " (previous messages: " ++ show ms ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut

domainNetworkDescription :: Inputs -> Moment Outputs
domainNetworkDescription (Inputs eOpen eMessage _ _ eHelp eQuit eUnknown) = do
  OpenOutput eoWrite        <- handleOpen $ OpenInput eOpen
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite        <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown
  return $ Outputs [eoWrite, emWrite, ehWrite, eqWrite, euWrite] [eqQuit]

go_3_m_3 :: IO ()
go_3_m_3 = mkGo domainNetworkDescription
