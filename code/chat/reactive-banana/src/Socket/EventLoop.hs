{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Socket.EventLoop (
    serverEventLoop
  , clientEventLoop
  ) where

import           Control.Concurrent           (forkFinally)
import           Control.Exception            (mask, try)
import           Control.Exception.Base       (SomeException (..))
import           Control.Monad                (forever, void)
import           System.IO                    (BufferMode (..), Handle,
                                               hGetLine, hSetBuffering,
                                               hSetNewlineMode,
                                               universalNewlineMode, hClose)

import           Control.Concurrent.Async     (race)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (readTChan)

import qualified Data.Text                    as T

import           Network                      (PortID (..), Socket, accept, listenOn, sClose)

import           Socket.InputSources          (ClientInputSources (..),
                                               ServerInputSources (..))
import           Util.IO                      (EventSource (..))

finally :: IO a -> (Either SomeException a -> IO ()) -> IO ()
finally action and_then =
  mask $ \restore ->
    try (restore action) >>= and_then

serverEventLoop :: Int -> ServerInputSources -> IO ()
serverEventLoop port (ServerInputSources esHandle) = do
  sock <- listenOn (PortNumber (fromIntegral port))
  finally
    (mainLoop sock)
    (cleanup sock)
  where
    mainLoop sock = do
      forever $ do
        (handle, _, _) <- accept sock
        hSetNewlineMode handle universalNewlineMode
        hSetBuffering handle LineBuffering

        fireEvent esHandle handle
    cleanup :: Socket -> Either SomeException () -> IO ()
    cleanup sock e = do
      print e
      sClose sock

clientEventLoop :: Handle -> ClientInputSources -> IO ()
clientEventLoop handle (ClientInputSources esOpened esRead esClosed refClosed) =
  void $ forkFinally
    mainLoop
    cleanup
  where
    mainLoop = do
      fireEvent esOpened ()
      void $ race checkQuit checkLine

    checkLine = forever $ do
      line <- hGetLine handle
      fireEvent esRead . T.pack $ line

    checkQuit =
      atomically . readTChan $ refClosed

    cleanup :: Either SomeException () -> IO ()
    cleanup e = do
      print e
      hClose handle
      case e of
        Left _ -> fireEvent esClosed ()
        _      -> return ()
