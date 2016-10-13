{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Http.EventLoop (
    eventLoop
  ) where

import           Data.Proxy             (Proxy (..))

import           Control.Monad.IO.Class (liftIO)

import           Servant                ((:<|>) (..), Server, serveSnap)
import           Snap                   (Handler, SnapletInit, addRoutes,
                                         defaultConfig, makeSnaplet,
                                         serveSnaplet)

import           Http.API               (API, Message (..), UserAPI)

data App = App

type AppHandler = Handler App App

api :: Proxy API
api = Proxy

server :: Server API AppHandler
server = login :<|> user
  where
    login u = return 1
    user i = message i :<|> tell i :<|> kick i :<|> fetch i :<|> quit i
    message i m = do
      liftIO . putStrLn $ "message: " ++ show m
      return ()
    tell i u m = do
      liftIO . putStrLn $ "tell: " ++ show u ++ " " ++ show m
      return ()
    kick i u = do
      liftIO . putStrLn $ "kick: " ++ show u
      return ()
    fetch i = do
      liftIO . putStrLn $ "fetch"
      return []
    quit i = do
      liftIO . putStrLn $ "quit"
      return ()

initApp :: SnapletInit App App
initApp = makeSnaplet "api" "" Nothing $ do
  addRoutes [("api", serveSnap api server)]
  return App

eventLoop :: IO ()
eventLoop = serveSnaplet defaultConfig initApp
