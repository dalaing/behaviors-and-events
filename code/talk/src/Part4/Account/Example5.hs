{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part3.Account.Example5 (
  go_3_a_5
  ) where

import Reactive.Banana

import Part3.Account.Common

data LimitInput = LimitInput {
    lieOpen      :: Event ()
  , lieUpgrade   :: Event ()
  , lieMessage   :: Event String
  , libSoftLimit :: Behavior Int
  , libHardLimit :: Behavior Int
  }

softLimitCheck :: Int -> Int -> Bool
softLimitCheck n x = x `mod` n == (n - 1)

hardLimitCheck :: Int -> Int -> Bool
hardLimitCheck n x = x >= (n - 1)

handleLimitPlebian :: MonadMoment m => LimitInput -> m LimitEvents
handleLimitPlebian (LimitInput _ _ eMessage bSoftLimit bHardLimit) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)

  let
    bSoft = softLimitCheck <$> bSoftLimit <*> bLines
    bHard = hardLimitCheck <$> bHardLimit <*> bLines
    eSoftLimitReached = () <$ whenE bSoft eMessage
    eHardLimitReached = () <$ whenE bHard eMessage

  return $ LimitEvents eSoftLimitReached eHardLimitReached

handleLimitPremium :: MonadMoment m => LimitInput -> m LimitEvents
handleLimitPremium _ =
  return $ LimitEvents never never

handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit li@(LimitInput eOpen eUpgrade _ _ _) = do
  LimitEvents eSoftPlebian eHardPlebian <- handleLimitPlebian li
  LimitEvents eSoftPremium eHardPremium <- handleLimitPremium li

  eSoftLimitReached <- switchE never . leftmost $ [
      eSoftPlebian <$ eOpen
    , eSoftPremium <$ eUpgrade
    ]

  eHardLimitReached <- switchE never . leftmost $ [
      eHardPlebian <$ eOpen
    , eHardPremium <$ eUpgrade
    ]

  return $ LimitEvents eSoftLimitReached eHardLimitReached

domainNetworkDescription :: Inputs -> Moment Outputs
domainNetworkDescription (Inputs eOpen eMessage eUpgrade eHelp eQuit eUnknown) = do
  OpenOutput eoWrite         <- handleOpen $ OpenInput eOpen
  LimitOutput elWrite elQuit <- fmap translateLimitEvents .
                                handleLimit $ LimitInput eOpen eUpgrade eMessage (pure 5) (pure 10)
  MessageOutput emWrite      <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite         <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit  <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite      <- handleUnknown $ UnknownInput eUnknown
  return $
    Outputs
      [eoWrite, elWrite, emWrite, ehWrite, eqWrite, euWrite]
      [elQuit, eqQuit]

go_3_a_5 :: IO ()
go_3_a_5 = mkGo domainNetworkDescription
