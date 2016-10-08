{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Components.Unknown (
    UnknownInput(..)
  , UnknownOutput(..)
  , parseUnknown
  , handleUnknown
  ) where

import           Control.Monad                (guard)
import           Data.Maybe                   (fromMaybe)

import qualified Data.Text                    as T

import           Reactive.Banana              (Event, MonadMoment)

import           Chat.Components.Help         (keywordHelp)
import           Chat.Components.Kick         (keywordKick)
import           Chat.Components.Notification (keywordFetch)
import           Chat.Components.Quit         (keywordQuit)
import           Chat.Components.Tell         (keywordTell)
import           Chat.Types.Config            (Config)

data UnknownInput = UnknownInput {
    uieUnknown :: Event T.Text
  }

data UnknownOutput = UnknownOutput {
    uoeWrite :: Event T.Text
  }

parseUnknown :: Config
             -> T.Text
             -> Maybe T.Text
parseUnknown c cmd =
  let
    keywords =
      fromMaybe [] (pure <$> keywordFetch c) ++
        [ keywordTell
        , keywordKick
        , keywordHelp
        , keywordQuit
        ]
  in do
    guard $ cmd `notElem` keywords
    return cmd

unknownMessage :: T.Text
               -> T.Text
unknownMessage s
  | T.null s =
    "Command can not be an empty string.\nType /help for options."
  | otherwise =
    T.concat ["Unknown command: ", s, ".\nType /help for options."]

handleUnknown :: MonadMoment m
              => UnknownInput
              -> m UnknownOutput
handleUnknown (UnknownInput eUnknown) =
  let
    eWrite = unknownMessage <$> eUnknown
  in
    return $ UnknownOutput eWrite
