{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Account.Example4 (
  go_3_a_4
  ) where

import Reactive.Banana

import Part3.Account.Common

data LimitInput = LimitInput {
    lieUpgrade   :: Event ()
  , lieMessage   :: Event String
  , libSoftLimit :: Behavior Int
  , libHardLimit :: Behavior Int
  }

softLimitCheck :: Int -> Int -> Bool
softLimitCheck n x = x `mod` n == (n - 1)

hardLimitCheck :: Int -> Int -> Bool
hardLimitCheck n x = x >= (n - 1)

handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit (LimitInput eUpgrade eMessage bSoftLimit bHardLimit) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)

  let
    bPlebianSoft = softLimitCheck <$> bSoftLimit <*> bLines
    bPremiumSoft = pure False
    bPlebianHard = hardLimitCheck <$> bHardLimit <*> bLines
    bPremiumHard = pure False

  bSoft <- switchB bPlebianSoft (bPremiumSoft <$ eUpgrade)
  bHard <- switchB bPlebianHard (bPremiumHard <$ eUpgrade)

  let
    eSoftLimitReached = () <$ whenE bSoft eMessage
    eHardLimitReached = () <$ whenE bHard eMessage
  return $ LimitEvents eSoftLimitReached eHardLimitReached

domainNetworkDescription :: MonadMoment m => Inputs -> m Outputs
domainNetworkDescription (Inputs eOpen eMessage eUpgrade eHelp eQuit eUnknown) = do
  OpenOutput eoWrite         <- handleOpen $ OpenInput eOpen
  LimitOutput elWrite elQuit <- fmap translateLimitEvents .
                                handleLimit $ LimitInput eUpgrade eMessage (pure 5) (pure 10)
  MessageOutput emWrite      <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite         <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit  <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite      <- handleUnknown $ UnknownInput eUnknown
  return $
    Outputs
      [eoWrite, elWrite, emWrite, ehWrite, eqWrite, euWrite]
      [elQuit, eqQuit]

go_3_a_4 :: IO ()
go_3_a_4 = mkGo domainNetworkDescription