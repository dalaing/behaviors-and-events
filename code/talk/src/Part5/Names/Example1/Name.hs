{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part5.Names.Example1.Name (
    NameInput(..)
  , NameOutput(..)
  , handleName
  , handleName2
  ) where

import qualified Data.Set        as S

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import Part5.Names.Example1.Common

data NameInput = NameInput {
    nieStart :: Event ()
  , nieRead  :: Event String
  , nibNames :: Behavior (S.Set String)
  }

data NameOutput = NameOutput {
    noeWrite :: Event String
  , noeName  :: Event String
  }

data NameError =
    EmptyName
  | MultiWordName String
  | IllegalCharInName String
  | NameInUse String
  deriving (Eq, Ord, Show)

nameErrorMessage :: NameError -> String
nameErrorMessage EmptyName =
  "Your name cannot be an empty string"
nameErrorMessage (MultiWordName _) =
  "Your name can only be a single word"
nameErrorMessage (IllegalCharInName _) =
  "Your name cannot contain the character '/'"
nameErrorMessage (NameInUse s) =
  "The name " ++ s ++ " is already in use"

nameInvalidMessage :: NameError -> String
nameInvalidMessage e =
  nameErrorMessage e ++ "\nPlease enter your name:"

processName :: S.Set String -> String -> Either NameError String
processName names line =
  case words line of
    [] ->
      Left EmptyName
    [n]
      | n `S.member` names ->
        Left $ NameInUse line
      | '/' `elem` n ->
        Left $ IllegalCharInName line
      | otherwise ->
        Right n
    _ ->
      Left $ MultiWordName line

handleName :: MonadMoment m => NameInput -> m NameOutput
handleName (NameInput eStart eRead bNames) = do
  let
    (eNameInvalid, eNameValid) = split $ processName <$> bNames <@> eRead
    eWrite = leftmost [
        "Welcome to the chat server.\nPlease enter your name:" <$ eStart
      , nameErrorMessage <$> eNameInvalid
      ]

  return $ NameOutput eWrite eNameValid

handleName2 :: MonadMoment m => NameInput -> (Event String, m NameOutput)
handleName2 (NameInput eStart eRead bNames) =
  let
    open =
      "Welcome to the chat server.\nPlease enter your name:" <$ eStart
    network = do
      let
        (eNameInvalid, eNameValid) = split $ processName <$> bNames <@> eRead
        eWrite = leftmost [
            "Welcome to the chat server.\nPlease enter your name:" <$ eStart
          , nameErrorMessage <$> eNameInvalid
          ]

      return $ NameOutput eWrite eNameValid
  in
    (open, network)
