{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Command.Domain (
    DomainInput(..)
  , DomainOutput(..)
  , fanOut
  , fanIn
  , pureCommandNetworkDescription
  ) where

import qualified Data.Set                      as S

import           Safe

import           Reactive.Banana

import           Part4.Command.Components.Message
import           Part4.Command.Components.Kick
import           Part4.Command.Components.Tell
import           Part4.Command.Components.Help
import           Part4.Command.Components.Quit
import           Part4.Command.Components.Unknown
import           Part4.Command.Types
import           Part4.Common.Util
import           Part4.Types
import           Part4.Types.Notification
import           Part4.Types.PrivateMessage

data DomainInput = DomainInput {
    dieFetch   :: Event ()
  , dieMessage :: Event Message
  , dieTell    :: Event PrivateMessage
  , dieKick    :: Event User
  , dieHelp    :: Event ()
  , dieQuit    :: Event ()
  , dieUnknown :: Event String
  , dibNames   :: Behavior (S.Set User)
  , dibName    :: Behavior User
  }

data DomainOutput = DomainOutput {
    doeWrite    :: [Event String]
  , doeClose    :: [Event ()]
  , doeNotifies :: [Event Notification]
  , doeFetch    :: Event ()
  , doeKick     :: Event User
  }

splitFirstWord :: String -> (String, String)
splitFirstWord s = maybe ("", "") f . headMay . words $ s
  where
    f r = (r, drop (1 + length r) s)

fanOut :: CommandInput -> DomainInput
fanOut (CommandInput eRead bNames bName) =
  let
    eReadNonEmpty =
      filterE (not . null) eRead

    isMessage =
      (/= "/") . take 1
    eMessage =
      filterE isMessage eReadNonEmpty

    isCommand =
      (== "/") . take 1
    eCommand =
      fmap (drop 1) . filterE isCommand $ eReadNonEmpty

    eWords = splitFirstWord <$> eCommand

    -- eWords :: Event (String, String)
    -- eFirstWord = fst <$> eWords
    -- eOtherWords = snd <$> eWords
    -- eDoubleFirst = (\x -> x ++ x) <$> eFirstWord
    -- eDoubleWords = unionWith (,) eDoubleFirst eOtherWords

    eTell =
      (uncurry PrivateMessage . splitFirstWord . snd)  <$> filterE ((== "tell") . fst) eWords
    eKick =
      snd <$> filterE ((== "kick") . fst) eWords

    eFetch      = () <$ filterE (== "fetch") eCommand
    eHelp      = () <$ filterE (== "help") eCommand
    eQuit      = () <$ filterE (== "quit") eCommand

    commands =
      ["fetch", "tell", "kick", "help", "quit"]
    eUnknown =
      (\(x,y) -> unwords [x, y]) <$> filterE ((`notElem` commands) . fst) eWords
  in
    DomainInput eFetch eMessage eTell eKick eHelp eQuit eUnknown bNames bName

fanIn :: DomainOutput -> CommandOutput
fanIn (DomainOutput eWrites eCloses eNotifies eFetch eKick) =
  let
    addLine x y = x ++ '\n' : y
    eCombinedWrites = foldr (unionWith addLine) never eWrites
    eCombinedCloses = () <$ leftmost eCloses
    eNotify = leftmost eNotifies
  in
    CommandOutput eCombinedWrites eCombinedCloses eNotify eFetch eKick

pureCommandNetworkDescription :: MonadMoment m => DomainInput -> m DomainOutput
pureCommandNetworkDescription di = do
  MessageOutput emNotify <- handleMessage $ MessageInput (dibName di) (dieMessage di)
  TellOutput etNotify etWrite <- handleTell $ TellInput (dibNames di) (dibName di) (dieTell di)
  KickOutput eKickValid ekNotify ekWrite <- handleKick $ KickInput (dibNames di) (dibName di) (dieKick di)
  HelpOutput ehWrite <- handleHelp $ HelpInput (dieHelp di)
  QuitOutput eqNotify eqClose <- handleQuit $ QuitInput (dibName di) (dieQuit di)
  UnknownOutput euWrite <- handleUnknown $ UnknownInput (dieUnknown di)

  let
    eWrites = [
        etWrite
      , ekWrite
      , ehWrite
      , euWrite
      ]
    eCloses = [
        eqClose
      ]
    eNotifies = [
        emNotify
      , etNotify
      , ekNotify
      , eqNotify
      ]
    eFetch =
      dieFetch di
  return $ DomainOutput eWrites eCloses eNotifies eFetch eKickValid
