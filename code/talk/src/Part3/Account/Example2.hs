{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Account.Example2 (
  go_3_a_2
  ) where

import Reactive.Banana

import Part3.Account.Common

data LimitInput = LimitInput {
    lieMessage   :: Event String
  , libAccount   :: Behavior AccountType
  , libSoftLimit :: Behavior Int
  , libHardLimit :: Behavior Int
  }

softLimitCheck :: Int -> AccountType -> Int -> Bool
softLimitCheck n Plebian x = x `mod` n == (n - 1)
softLimitCheck _ Premium _ = False

hardLimitCheck :: Int -> AccountType -> Int -> Bool
hardLimitCheck n Plebian x = x >= (n - 1)
hardLimitCheck _ Premium _ = False

handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit (LimitInput eMessage bAccount bSoftLimit bHardLimit) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimitReached = softLimitCheck <$> bSoftLimit <*> bAccount <*> bLines
    bHardLimitReached = hardLimitCheck <$> bHardLimit <*> bAccount <*> bLines

    eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
    eHardLimitReached = () <$ whenE bHardLimitReached eMessage
  return $ LimitEvents eSoftLimitReached eHardLimitReached

domainNetworkDescription :: MonadMoment m => Inputs -> m Outputs
domainNetworkDescription (Inputs eOpen eMessage eUpgrade eHelp eQuit eUnknown) = do
  OpenOutput eoWrite         <- handleOpen $ OpenInput eOpen
  UpgradeOutput bAccount     <- handleUpgrade $ UpgradeInput eUpgrade
  LimitOutput elWrite elQuit <- fmap translateLimitEvents .
                                handleLimit $ LimitInput eMessage bAccount (pure 5) (pure 10)
  MessageOutput emWrite      <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite         <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit  <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite      <- handleUnknown $ UnknownInput eUnknown
  return $
    Outputs
      [eoWrite, elWrite, emWrite, ehWrite, eqWrite, euWrite]
      [elQuit, eqQuit]

go_3_a_2 :: IO ()
go_3_a_2 = mkGo domainNetworkDescription
