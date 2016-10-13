{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module Socket.Network (
    network
  , network2
  ) where

import           Control.Monad                (unless, void)
import           Data.IORef                   (readIORef, writeIORef)
import           Data.List                    (group, sortOn)
import           Data.Maybe                   (mapMaybe)
import           Data.Traversable             (traverse)
import           Data.Tuple                   (swap)
import           System.IO                    (Handle, hClose, hPutStrLn)

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (writeTChan)

import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.Text                    as T

import           Reactive.Banana              (Event, Moment, MonadMoment (..),
                                               accumB, accumE, filterJust,
                                               never, observeE, switchB,
                                               switchE, unionWith, unions,
                                               (<@>))
import           Reactive.Banana.Frameworks   (MomentIO, execute, liftIOLater, liftIO,
                                               reactimate)

import           Chat.Network.Client          (ClientInput (..),
                                               ClientOutput (..), clientNetwork)
import           Chat.Network.Client.Types    (InputIO (..), OutputIO (..))
import           Chat.Types.Config            (Config (..))
import           Chat.Types.Name              (NameType (..))
import           Chat.Types.Notification      (NotificationType (..))
import           Socket.EventLoop             (clientEventLoop)
import           Socket.InputSources          (ClientInputSources (..),
                                               ServerInputSources (..),
                                               mkClientInputSources)
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
    closeOnce = do
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

network :: ServerInputSources -> MomentIO ()
network (ServerInputSources esHandle) = mdo
  let
    config = Config Interactive Stream
    bLimit = pure 100
    emptyInputIO = InputIO never never never

  -- build a map of ids to InputIOs
  -- - this is where we kick over the client event loop in the socket version
  -- build a map of ids to (OutputIO -> MomentIO ())
  -- - possibly to MomentIO (Map Int a), for use with the testing version

  -- pure bit:
  -- map from (map id InputIO) to (map id OutputIO)
  --  - possibly with a traverse
  --  - need to handle the case where the map is changing
  -- also need to provide an event with a set of ids to remove
  --  - so that we can clean up the input / output maps
  --  - there might be a better way to manage that

  -- make it all happen:
  -- void . sequence $ intersectWith ($) (map id (OutputIO -> MomentIO ())) (map id OutputIO)


  eHandle <- registerEvent esHandle

  bId <- accumB 0 ((+ 1) <$ eHandle)

  eOut <- execute $ (\i h -> mkClient config (ClientInput i bLimit bNameIdMap eNotifyIn eKickIn emptyInputIO) h) <$> bId <@> eHandle

  -- Issues
  -- - not filtering the tell messages yet
  -- - kick with no user name is accepted
  -- - we should inform the client when the server goes down
  --   - at the moment we can kill the server and it seems to keep
  --     rolling on, which is worth looking into...
  --   - need to generally clean up in that case as well

  let
    bulkRemove s m = S.foldr M.delete m s

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

data PureIn =
  PureIn {
    pieAdd :: Event (Int, InputIO)

  }

data PureOut =
  PureOut {
    poeClients :: Event (M.Map Int OutputIO)
  , poeClose   :: Event (S.Set Int)
  }

-- should we take a map in?
-- that would possibly make the testing easier...
pureNetwork :: PureIn -> Moment PureOut
pureNetwork (PureIn eAdd) = mdo
  let
    config = Config Interactive Stream
    bLimit = pure 100

    add i input = fmap (\x -> (i, x)) . clientNetwork config $ ClientInput i bLimit bNameIdMap eNotifyIn eKickIn input
    eOut = observeE $ uncurry add <$> eAdd

    bulkRemove s m = S.foldr M.delete m s

  eClientMap <- accumE M.empty . unions $ [
      uncurry M.insert <$> eOut
    , bulkRemove <$> eClose
    ]

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

  return $ PureOut eOutMap eClose

mkClient2 :: Event (M.Map Int OutputIO) -> Int -> Handle -> MomentIO ((Int, InputIO))
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
  PureOut eOMap eClose <- liftMoment . pureNetwork $ PureIn eI

  return ()

mkClient3 :: Handle -> OutputIO -> MomentIO InputIO
mkClient3 handle o = do
  cis <- mkClientInputSources
  input <- handleInput cis
  handleOutput handle cis o
  liftIOLater $ clientEventLoop handle cis
  return input

{-
For testing, we might want a bit more separation
- create a map from id to things that handle io
- have them fire event (id, value) for the various values we care about
- have another map with the pure state-per-id, and route the events into the map based on the id
- hopefully that means we can test the central bit with (id, value) pairs as inputs

- modelling the connection will be interesting

Probably a good idea to get the above going first though.

Something like this also has a better chance of generalizing to the web based form.
-}

{-
We want something we can reuse in at least 3 contexts:
- sockets
- http
- testing

Input events are (Int, InputCmd) like
In
  (1, Open)
  (1, Read "hi")
  (1, Close)
Out
  (2, Write "A has joined)
  (3, Write "A has joined)
  (2, Write "<A> hi)
  (3, Write "<A> hi)
  (2, Write "A has quit)
  (3, Write "A has quit)

This is not the normal open - this is after the name phase
It is more like
  (1, Create "A")

This re-introduces the problem of using eName to do a switch
while also using it as an open event for the second phase.

Hmm

(1, Open)
  (1, Write "What is your name?")
(1, Read "A")
  (1, Write "A joined")
  (2, Write "A joined")
  (3, Write "A joined")
-}
