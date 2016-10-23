{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
module Local.Network (
    network
  ) where

import           Data.IORef                          (writeIORef)

import qualified Data.Map                            as M
import qualified Data.Text                           as T

import           Reactive.Banana                     (Moment, MonadMoment (..), filterJust, accumB, never)
import           Reactive.Banana.Frameworks          (MomentIO, reactimate)

import           Chat.Network.Client                 (ClientInput (..),
                                                      ClientOutput (..),
                                                      clientNetwork)
import           Chat.Network.Types                  (LineInput (..),
                                                      LineOutput (..),
                                                      LineInputSources(..))
import           Chat.Types.Config                   (Config (..))
import           Chat.Types.Name                     (NameType (..))
import           Chat.Types.Notification             (NotificationType (..))
import           Util.IO                             (EventSource (..))

handleInput :: EventSource e m => LineInputSources e -> MomentIO LineInput
handleInput (LineInputSources esOpen esRead esClose _) = do
  eOpen  <- registerEvent esOpen
  eRead  <- registerEvent esRead
  eClose <- registerEvent esClose
  return $ LineInput eOpen eRead eClose

handleOutput :: LineInputSources e -> LineOutput -> MomentIO ()
handleOutput (LineInputSources _ _ _ refClose) (LineOutput eWrite eClose) = do
  reactimate $ putStrLn . T.unpack <$> eWrite
  reactimate $ writeIORef refClose True <$ eClose

network :: EventSource e m => LineInputSources e -> MomentIO ()
network is = do
  i <- handleInput is
  o <- liftMoment $ network' i
  handleOutput is o

network' :: LineInput -> Moment LineOutput
network' i = mdo
  let
    config = Config Interactive Stream
    myId = 2
    bLimit = pure 100
    clientInput = ClientInput myId bLimit bNameIdMap (pure <$> eNotify) never i

  bNameIdMap <- accumB (M.fromList [("admin", 1)]) $
    (\n -> M.insert n myId) <$> filterJust eName

  ClientOutput _ eName eNotify _ o <- clientNetwork config clientInput
  return o

