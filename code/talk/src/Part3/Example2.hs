{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Example2 (
  go_3_2
  ) where

import Reactive.Banana

import Part3.Common

data MessageInput  = MessageInput  { mieRead  :: Event String }
data MessageOutput = MessageOutput { moeWrite :: Event String }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    bLastLength = length <$> bMessages
    format l m = m ++ " (last message length: " ++ show l ++ ")"
    eOut = format <$> bLastLength <@> eMessage
  return $ MessageOutput eOut

networkDescription :: Inputs -> Moment Outputs
networkDescription (Inputs eOpen (ReadInputs eMessage _ _ eHelp eUnknown eQuit)) = do
  OpenOutput eoWrite        <- handleOpen $ OpenInput eOpen
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite        <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown
  return $ Outputs (WriteOutputs eoWrite emWrite ehWrite euWrite eqWrite) eqQuit

go_3_2 :: IO ()
go_3_2 =
  mkGo . mkNetwork . handleIO $ networkDescription
