{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Account.Example1 (
  go_3_a_1
  ) where

import Reactive.Banana

import Part3.Account.Common

data LimitInput = LimitInput {
    lieMessage :: Event String
  , libAccount :: Behavior AccountType
  }

softLimitCheck :: AccountType -> Int -> Bool
softLimitCheck Plebian x = x `mod` 5 == 4
softLimitCheck Premium x = False

hardLimitCheck :: AccountType -> Int -> Bool
hardLimitCheck Plebian x = x >= 9
hardLimitCheck Premium x = False

handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit (LimitInput eMessage bAccount) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimitReached = softLimitCheck <$> bAccount <*> bLines
    bHardLimitReached = hardLimitCheck <$> bAccount <*> bLines

    eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
    eHardLimitReached = () <$ whenE bHardLimitReached eMessage
  return $ LimitEvents eSoftLimitReached eHardLimitReached

domainNetworkDescription :: Inputs -> Moment Outputs
domainNetworkDescription (Inputs eOpen eMessage eUpgrade eHelp eQuit eUnknown) = do
  OpenOutput eoWrite         <- handleOpen $ OpenInput eOpen
  UpgradeOutput bAccount     <- handleUpgrade $ UpgradeInput eUpgrade
  LimitOutput elWrite elQuit <- fmap translateLimitEvents .
                                handleLimit $ LimitInput eMessage bAccount
  MessageOutput emWrite      <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite         <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit  <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite      <- handleUnknown $ UnknownInput eUnknown
  return $
    Outputs
      [eoWrite, elWrite, emWrite, ehWrite, eqWrite, euWrite]
      [elQuit, eqQuit]

go_3_a_1 :: IO ()
go_3_a_1 = mkGo domainNetworkDescription
