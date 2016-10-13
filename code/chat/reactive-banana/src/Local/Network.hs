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
import           Chat.Network.Types                  (InputIO (..),
                                                      OutputIO (..),
                                                      InputSources(..))
import           Chat.Types.Config                   (Config (..))
import           Chat.Types.Name                     (NameType (..))
import           Chat.Types.Notification             (NotificationType (..))
import           Util.IO                             (EventSource (..))

handleInput :: EventSource e m => InputSources e -> MomentIO InputIO
handleInput (InputSources esOpen esRead esClose _) = do
  eOpen  <- registerEvent esOpen
  eRead  <- registerEvent esRead
  eClose <- registerEvent esClose
  return $ InputIO eOpen eRead eClose

handleOutput :: InputSources e -> OutputIO -> MomentIO ()
handleOutput (InputSources _ _ _ refClose) (OutputIO eWrite eClose) = do
  reactimate $ putStrLn . T.unpack <$> eWrite
  reactimate $ writeIORef refClose True <$ eClose

network :: EventSource e m => InputSources e -> MomentIO ()
network is = do
  i <- handleInput is
  o <- liftMoment $ network' i
  handleOutput is o

network' :: InputIO -> Moment OutputIO
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

