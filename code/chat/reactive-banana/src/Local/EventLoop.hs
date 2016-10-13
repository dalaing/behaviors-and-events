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

import           Control.Monad             (unless)
import           Data.IORef                (readIORef)

import           Control.Monad.IO.Class    (liftIO)

import qualified Data.Text                 as T

import           System.Console.Haskeline

import           Chat.Network.Types        (InputSources(..))
import           Util.IO                   (EventSource (..))

eventLoop :: EventSource e m => InputSources e -> IO ()
eventLoop (InputSources esOpen esRead esClosed refClose) = do
    fireEvent esOpen ()
    runInputT defaultSettings loop
  where
    loop = do
      l <-
        handle (\Interrupt -> return Nothing) .
        withInterrupt $
        getInputLine "> "
      liftIO $ maybe (fireEvent esClosed ()) (fireEvent esRead . T.pack) l

      c <- liftIO $ readIORef refClose
      unless c loop
