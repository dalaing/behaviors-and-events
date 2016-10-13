{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module Socket.Network where {-(

    network
  , network2
  ) where -}

import           Data.List                    (group, sortOn)
import           Data.Maybe                   (mapMaybe)
import           Data.Traversable             (traverse)
import           Data.Tuple                   (swap)
import           System.IO                    (Handle, hPutStrLn)

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (writeTChan)

import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.Text                    as T

import           Reactive.Banana              (Event, MonadMoment (..), accumB,
                                               accumE, filterJust, never,
                                               switchB, switchE, unionWith,
                                               unions, (<@>))
import           Reactive.Banana.Frameworks   (MomentIO, execute, liftIOLater,
                                               reactimate)

import           Chat.Network.Client          (ClientInput (..),
                                               ClientOutput (..), clientNetwork)
import           Chat.Network.Server          (ServerOutput (..),
                                               serverNetworkFromAddEvent)
import           Chat.Network.Types           (InputIO (..), OutputIO (..))
import           Chat.Types.Config            (Config (..))
import           Chat.Types.Name              (NameType (..))
import           Chat.Types.Notification      (NotificationType (..))
import           Socket.EventLoop             (clientEventLoop)
import           Socket.InputSources          (ClientInputSources (..),
                                               ServerInputSources (..),
                                               mkClientInputSources)
import           Util                         (bulkRemove)
import           Util.IO                      (EventSource (..))
import           Util.Switch                  (Switch (..))

handleInput :: ClientInputSources -> MomentIO InputIO
handleInput (ClientInputSources esOpen esRead esClose _) = do
  eOpen  <- registerEvent esOpen
  eRead  <- registerEvent esRead
  eClose <- registerEvent esClose
  return $ InputIO eOpen eRead eClose

handleOutput :: Handle -> ClientInputSources -> OutputIO -> MomentIO ()
handleOutput h (ClientInputSources _ _ _ refClose) (OutputIO eWrite eClose) = do
    reactimate $ hPutStrLn h . T.unpack <$> eWrite
    reactimate $ closeOnce <$ eClose
  where
    closeOnce =
      atomically . writeTChan refClose $ ()

mkClient :: Config -> ClientInput -> Handle -> MomentIO ClientOutput
mkClient config ci handle = do
  cis <- mkClientInputSources
  i <- handleInput cis

  let
    ci' = ci { ciIO = i}

  co <- liftMoment $ clientNetwork config ci'

  handleOutput handle cis (coIO co)

  liftIOLater $ clientEventLoop handle cis

  return co

-- Issues
-- - not filtering the tell messages yet
-- - kick with no user name is accepted
-- - we should inform the client when the server goes down
--   - at the moment we can kill the server and it seems to keep
--     rolling on, which is worth looking into...
--   - need to generally clean up in that case as well

network :: ServerInputSources -> MomentIO ()
network (ServerInputSources esHandle) = mdo
  let
    config = Config Interactive Stream
    bLimit = pure 100
    emptyInputIO = InputIO never never never

  eHandle <- registerEvent esHandle

  bId <- accumB 0 ((+ 1) <$ eHandle)

  eOut <- execute $ (\i h -> mkClient config (ClientInput i bLimit bNameIdMap eNotifyIn eKickIn emptyInputIO) h) <$> bId <@> eHandle

  eClientMap <- accumE M.empty . unions $ [
      M.insert <$> bId <@> eOut
    , bulkRemove <$> eClose
    ]

  -- I think this is forcing the initial connection to work
  -- We might get this for free when we separate out the IO with some maps
  -- Let's leave it out for a while and see what happens...
  -- reactimate $ print . M.size <$> eClientMap

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

  return ()

mkClient2 :: Event (M.Map Int OutputIO) -> Int -> Handle -> MomentIO (Int, InputIO)
mkClient2 mOut i handle = do
  cis <- mkClientInputSources
  input <- handleInput cis

  let
    emptyOutput = OutputIO never never
  -- There is a delay in the switch, such that the last client to join
  -- doesn't see any output events happening
  --
  -- There are probably ways to adapt handleOutput to work with
  -- an Event OutputIO rather than an OutputIO
  -- fmap and execute?
  output <- switch emptyOutput . filterJust . fmap (M.lookup i) $ mOut
  handleOutput handle cis output
  -- let
  --  eOut = filterJust . fmap (M.lookup i) $ mOut
  --execute $ handleOutput handle cis <$> eOut

  -- this is also happening earlier than we'd like - the open event
  -- seems to be happening before the network can get hold of it
  liftIOLater $ clientEventLoop handle cis

  return (i, input)

network2 :: ServerInputSources -> MomentIO ()
network2 (ServerInputSources esHandle) = mdo
  eHandle <- registerEvent esHandle
  bId <- accumB 0 ((+ 1) <$ eHandle)
  eI <- execute $ mkClient2 eOMap <$> bId <@> eHandle
  ServerOutput eOMap _ <- liftMoment . serverNetworkFromAddEvent $ eI
  return ()

mkClient3 :: Handle -> OutputIO -> MomentIO InputIO
mkClient3 handle o = do
  cis <- mkClientInputSources
  input <- handleInput cis
  handleOutput handle cis o
  liftIOLater $ clientEventLoop handle cis
  return input

network3 :: ServerInputSources -> MomentIO ()
network3 (ServerInputSources esHandle) = mdo
  eHandle <- registerEvent esHandle
  bId <- accumB 0 ((+ 1) <$ eHandle)

  eI <- execute $ mkClient2 eOMap <$> bId <@> eHandle

  ServerOutput eOMap _ <- liftMoment . serverNetworkFromAddEvent $ eI

  return ()

