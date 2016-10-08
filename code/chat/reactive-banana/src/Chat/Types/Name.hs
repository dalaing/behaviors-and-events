{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Types.Name (
    Name
  , NameError
  , nameErrorText
  , checkName
  , checkNameAlreadyInUse
  , checkNameNotInUse
  ) where

import qualified Data.Set      as S (Set, member, notMember)
import qualified Data.Text     as T

type Name = T.Text

data NameError =
    Empty
  | MultipleWords Name
  | IllegalCharacter Name
  | NotInUse Name
  | AlreadyInUse Name
  deriving (Eq, Ord, Show)

nameErrorText :: NameError
              -> T.Text
nameErrorText ne =
  case ne of
    Empty ->
      "Your name cannot be an empty string"
    MultipleWords _ ->
      "Your name can only be a single word"
    IllegalCharacter _ ->
      "Your name cannot contain the character '/'"
    NotInUse s ->
      T.concat ["The name ", s, " is not in use"]
    AlreadyInUse s ->
      T.concat ["The name ", s, " is already in use"]

-- TODO consider changing these to work with MonadError if we end up using
-- them in a wider range of contexts

checkName :: Name
          -> Either NameError Name
checkName n
  | T.null n =
    Left Empty
  | length (T.words n) /= 1 =
    Left $ MultipleWords n
  | T.any (== '/') n =
    Left $ IllegalCharacter n
  | otherwise =
    -- TODO trim any leading or trailing whitespace
    --   or make leading or trailing whitespace an error
    Right n

checkNameNotInUse :: S.Set Name
                  -> Name
                  -> Either NameError Name
checkNameNotInUse names n = do
  n' <- checkName n
  if S.member n names
  then Left $ AlreadyInUse n
  else Right n'

checkNameAlreadyInUse :: S.Set Name
                      -> Name
                      -> Either NameError Name
checkNameAlreadyInUse names n = do
  n' <- checkName n
  if S.notMember n names
  then Left $ NotInUse n
  else Right n'
