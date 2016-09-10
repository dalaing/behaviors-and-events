{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part4.Types.NameError (
    NameError(..)
  , nameErrorMessage
  , checkValidName
  , checkValidNameInUse
  , checkValidNameNotInUse
  ) where

import qualified Data.Set as S

data NameError =
    EmptyName
  | MultiWordName String
  | IllegalCharInName String
  | NameInUse String
  | NameNotInUse String
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
nameErrorMessage (NameNotInUse s) =
  "The name " ++ s ++ " is not in use"

checkValidName :: String -> Either NameError String
checkValidName name =
  case words name of
    [] ->
      Left EmptyName
    [n]
      | '/' `elem` n ->
        Left $ IllegalCharInName name
      | otherwise ->
        Right n
    _ ->
      Left $ MultiWordName name

checkValidNameNotInUse :: S.Set String -> String -> Either NameError String
checkValidNameNotInUse names line = do
  name <- checkValidName line
  if name `S.member` names
  then Left $ NameInUse line
  else Right name

checkValidNameInUse :: S.Set String -> String -> Either NameError String
checkValidNameInUse names line = do
  name <- checkValidName line
  if name `S.notMember` names
  then Left $ NameNotInUse line
  else Right name
