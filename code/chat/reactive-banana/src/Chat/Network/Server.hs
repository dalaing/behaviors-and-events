{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module Chat.Network.Server (
    ServerInput(..)
  , ServerOutput(..)
  , serverNetwork
  , serverNetworkFromAddEvent
  ) where

import           Data.List               (group, sortOn)
import           Data.Maybe              (mapMaybe)
import           Data.Tuple              (swap)

import qualified Data.Map                as M
import qualified Data.Set                as S

import           Reactive.Banana         (Event, Moment, accumE, never,
                                          observeE, switchB, switchE, unionWith,
                                          unions)

import           Chat.Network.Client     (ClientInput (..), ClientOutput (..),
                                          clientNetwork)
import           Chat.Network.Types      (InputIO (..), OutputIO (..))
import           Chat.Types.Config       (Config (..))
import           Chat.Types.Name         (NameType (..))
import           Chat.Types.Notification (NotificationType (..))
import           Util                    (bulkRemove)

data ServerInput =
  ServerInput {
    sieClients :: Event (M.Map Int InputIO)
  }

data ServerOutput =
  ServerOutput {
    soeClients :: Event (M.Map Int OutputIO)
  , soeClose :: Event (S.Set Int)
  }

serverNetworkFromAddEvent :: Event (Int, InputIO) -> Moment ServerOutput
serverNetworkFromAddEvent eAdd = mdo
  eInMap <- accumE M.empty . unions $ [
      uncurry M.insert <$> eAdd
    , bulkRemove <$> eClose
    ]
  so@(ServerOutput _ eClose) <- serverNetwork $ ServerInput eInMap
  return so

serverNetwork :: ServerInput -> Moment ServerOutput
serverNetwork (ServerInput eInMap) = mdo
  let
    config = Config Interactive Stream
    bLimit = pure 100

  let
    eClientMap = observeE $ sequence . M.mapWithKey (\i -> clientNetwork config . ClientInput i bLimit bNameIdMap eNotifyIn eKickIn) <$> eInMap

  bIdNameMap <- switchB (pure M.empty) . fmap (traverse cobName) $ eClientMap
  let
    flipper =
      M.fromList . fmap head . group . sortOn fst . fmap swap . mapMaybe sequence . M.toList
    bNameIdMap = flipper <$> bIdNameMap

  let
    eeNotifications = (foldr (unionWith (++)) never . fmap (fmap pure . coeNotifyOut) . M.elems) <$> eClientMap
  eNotifyIn <- switchE eeNotifications

  let
    eeKicks = (foldr (unionWith S.union) never . fmap (fmap S.singleton . coeKick) . M.elems) <$> eClientMap
  eKickIn <- switchE eeKicks

  let
    eeCloses = (foldr (unionWith S.union) never . M.mapWithKey (\i v -> S.singleton i <$ (oeClose . coIO $ v))) <$> eClientMap
  eClose <- switchE eeCloses

  let
    eOutMap = fmap coIO <$> eClientMap

  return $ ServerOutput eOutMap eClose
