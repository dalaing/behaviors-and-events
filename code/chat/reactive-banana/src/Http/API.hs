{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Http.API (
    API
  , UserAPI
  , Message(..)
  ) where

import GHC.Generics (Generic)

import qualified Data.Text as T

import Data.Aeson (FromJSON, ToJSON)

import Servant (Capture, ReqBody, Get, Post, Delete, JSON, (:>), (:<|>))

import Chat.Types.Notification (Notification)

data Message = Message T.Text
  deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message

type UserAPI =
  -- send a message
       "message" :> ReqBody '[JSON] Message :> Post '[JSON] ()
  -- send a private message
  :<|> "tell" :> Capture "name" T.Text :> ReqBody '[JSON] Message :> Post '[JSON] ()
  -- kick a user
  :<|> "kick" :> Capture "name" T.Text :> Post '[JSON] ()
  -- get notifications
  :<|> "fetch" :> Get '[JSON] [Notification]
  -- log out
  :<|> Delete '[JSON] ()

type API =
       "login" :> Capture "name" T.Text :> Post '[JSON] Int
  :<|> "user" :> Capture "id" Int :> UserAPI
