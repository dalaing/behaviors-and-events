{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module Test where

import qualified Data.Map                   as M
import qualified Data.Text                  as T

import           Reactive.Banana            (Event, Moment, MonadMoment (..),
                                             accumE, filterE, filterJust, never,
                                             switchE, unionWith, unions)
import           Reactive.Banana.Frameworks (MomentIO, reactimate)

import           Chat.Network.Server        (ServerInput (..),
                                             ServerOutput (..), serverNetwork)
import           Chat.Network.Types         (LineInput (..), LineOutput (..))

import           Util                       (bulkRemove, leftmost)


data TestIn =
    TIOpen Int
  | TIRead Int T.Text
  | TIClose Int
  deriving (Eq, Ord, Show)

mergeInput :: LineInput -> LineInput -> LineInput
mergeInput (LineInput eOpen1 eRead1 eClose1) (LineInput eOpen2 eRead2 eClose2) =
  LineInput
    (unionWith const eOpen1 eOpen2)
    (unionWith const eRead1 eRead2)
    (unionWith const eClose1 eClose2)

inToMap :: Event TestIn -> Event (Int, LineInput)
inToMap eIn =
  let
    mOpen (TIOpen i) = Just i
    mOpen _          = Nothing
    -- Event Int
    eOpen = filterJust $ mOpen <$> eIn
    mapOpen = M.fromList $ (\i -> (i, LineInput (() <$ filterE (== i) eOpen) never never)) <$> [0..]

    eiOpen = (\i -> (i, LineInput (() <$ filterE (== i) eOpen) never never)) <$> eOpen

    mRead (TIRead i t) = Just (i, t)
    mRead _            = Nothing
    eRead = filterJust $ mRead <$> eIn
    eiRead = (\(i, _) -> (i, LineInput never (snd <$> filterE ((== i) . fst) eRead) never)) <$> eRead

    mClose (TIClose i) = Just i
    mClose _           = Nothing
    eClose = filterJust $ mClose <$> eIn
    eiClose = (\i -> (i, LineInput never never (() <$ filterE (== i) eClose))) <$> eClose

  in
    leftmost [eiOpen, eiRead, eiClose]

data TestOut =
    TOWrite Int T.Text
  | TOClose Int
  deriving (Eq, Ord, Show)

-- can we rig up instance (Ord k, Switch a) => Switch (M.Map k a)?
-- switch :: map -> e (map) -> m (map)
-- would need to handle elements being added to and removed from
-- the map

mapToOut :: Event (M.Map Int LineOutput) -> Moment (Event [TestOut])
mapToOut eOutMap = do
  let
    fWrites = M.foldr (unionWith (++)) never . M.mapWithKey (\i -> fmap (pure . TOWrite i) . loeWrite)
    fCloses = M.foldr (unionWith (++)) never . M.mapWithKey (\i -> fmap (const $ pure $ TOClose i) . loeClose)
    eeOut = (\m -> unionWith (++) (fWrites m) (fCloses m)) <$> eOutMap

  switchE eeOut

test :: Event TestIn -> MomentIO (Event [TestOut])
test eIn = mdo

  eInMap <- accumE M.empty . unions $ [
      uncurry (M.insertWith mergeInput) <$> inToMap eIn
    , bulkRemove <$> eClose
    ]

  reactimate $ (\m -> putStrLn $ "in size " ++ (show . M.size $ m)) <$> eInMap

  ServerOutput eOutMap eClose <- liftMoment $ serverNetwork $ ServerInput eInMap

  reactimate $ (\m -> putStrLn $ "out size " ++ (show . M.size $ m)) <$> eOutMap

  liftMoment $ mapToOut eOutMap
