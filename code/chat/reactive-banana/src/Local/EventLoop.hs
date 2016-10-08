{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Local.EventLoop (
    eventLoop
  ) where

import           Control.Monad            (unless)
import           Data.IORef               (readIORef)

import           Control.Monad.IO.Class   (liftIO)

import qualified Data.Text                as T

import           System.Console.Haskeline

import           Local.InputSources       (InputSources (..))
import           Util.IO                  (EventSource (..))

eventLoop :: InputSources -> IO ()
eventLoop (InputSources esOpen esRead esClosed refClose) = do
    fire esOpen ()
    runInputT defaultSettings loop
  where
    loop = do
      l <-
        handle (\Interrupt -> return Nothing) .
        withInterrupt $
        getInputLine "> "
      liftIO $ maybe (fire esClosed ()) (fire esRead . T.pack) l

      c <- liftIO $ readIORef refClose
      unless c loop
