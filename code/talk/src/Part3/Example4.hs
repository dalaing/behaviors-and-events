{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Example4 (
  go_3_4
  ) where

import Reactive.Banana

import Part3.Common

data MessageInput  = MessageInput  { mieRead  :: Event String }
data MessageOutput = MessageOutput { moeWrite :: Event String }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $
    (\x xs -> x : xs) <$> eMessage

  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessages eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]

  return $ MessageOutput eOut

networkDescription :: Inputs -> Moment Outputs
networkDescription (Inputs eOpen (ReadInputs eMessage _ _ eHelp eUnknown eQuit)) = do
  OpenOutput eoWrite        <- handleOpen $ OpenInput eOpen
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite        <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown
  return $ Outputs (WriteOutputs eoWrite emWrite ehWrite euWrite eqWrite) eqQuit

go_3_4 :: IO ()
go_3_4 =
  mkGo . mkNetwork . handleIO $ networkDescription
