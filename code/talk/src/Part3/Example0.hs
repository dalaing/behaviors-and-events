{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module Part3.Example0 (
  ) where

import Control.Monad.Fix

import Reactive.Banana

import Part3.Common.Util

data LoginState =
    LoggedIn
  | LoggedOut
  deriving (Eq, Ord, Show)

data LoginError =
    AlreadyLoggedIn
  | NotLoggedIn
  deriving (Eq, Ord, Show)

logIn :: LoginState -> Either LoginError LoginState
logIn LoggedIn  = Left AlreadyLoggedIn
logIn LoggedOut = Right LoggedIn

logOut :: LoginState -> Either LoginError LoginState
logOut LoggedOut = Left NotLoggedIn
logOut LoggedIn  = Right LoggedOut

data LoginInputs = LoginInputs {
    lieLogIn  :: Event ()
  , lieLogOut :: Event ()
  }

data LoginOutputs = LoginOutputs {
    lobState :: Behavior LoginState
  , loeError :: Event LoginError
  }

logInHandler1 :: LoginInputs -> Moment LoginOutputs
logInHandler1 (LoginInputs eLogIn eLogOut) = do
  bState <- stepper LoggedOut . leftmost $ [
      LoggedIn  <$ eLogIn                                      -- (1)
    , LoggedOut <$ eLogOut                                     -- (2)
    ]
  return $ LoginOutputs bState never

logInHandler2 :: LoginInputs -> Moment LoginOutputs
logInHandler2 (LoginInputs eLogIn eLogOut) = mdo
  bLogInState <- stepper LoggedOut eLogInState
  let
    (eLogInError, eLogInState) = split . leftmost $ [
        logIn  <$> bLogInState <@ eLogIn
      , logOut <$> bLogInState <@ eLogOut
      ]
  return $ LoginOutputs bLogInState eLogInError

logInHandler3 :: (MonadMoment m, MonadFix m) => LoginInputs -> m LoginOutputs
logInHandler3 (LoginInputs eLogIn eLogOut) = mdo
  bLogInState <- stepper LoggedOut eLogInState
  let
    (eLogInError, eLogInState) = split . leftmost $ [
        logIn  <$> bLogInState <@ eLogIn
      , logOut <$> bLogInState <@ eLogOut
      ]
  return $ LoginOutputs bLogInState eLogInError

logInHandler4 :: LoginInputs -> Moment LoginOutputs
logInHandler4 (LoginInputs eLogIn eLogOut) = do
  rec
    bLogInState <- stepper LoggedOut eLogInState
    let
      (eLogInError, eLogInState) = split . leftmost $ [
          logIn  <$> bLogInState <@ eLogIn
        , logOut <$> bLogInState <@ eLogOut
        ]
  return $ LoginOutputs bLogInState eLogInError
