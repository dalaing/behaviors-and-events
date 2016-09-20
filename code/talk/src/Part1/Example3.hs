{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Part1.Example3 (
    go_1_3
  , test_1_3_counter
  , test_1_3_combined
  ) where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)

import Reactive.Banana
import Reactive.Banana.Frameworks

data EventSource a = EventSource {
    addHandler :: AddHandler a
  , fire       :: a -> IO ()
  }

mkEventSource :: IO (EventSource a)
mkEventSource =
  uncurry EventSource <$> newAddHandler

multiple :: Int -> Event Int -> Event Int
multiple m =
  filterE (\x -> x `mod` m == 0)

counter :: MonadMoment m => Event () -> m (Event Int)
counter eTick = accumE 0 ((+ 1) <$ eTick)

importantWork :: Event Int -> Event String
importantWork eCount =
  let
    eFizz = "Fizz" <$ multiple 3 eCount
    eBuzz = "Buzz" <$ multiple 5 eCount
    eFizzBuzz = unionWith (++) eFizz eBuzz
  in
    eFizzBuzz

combined :: MonadMoment m => Event () -> m (Event String)
combined eTick = do
  eCount <- counter eTick
  return $ importantWork eCount

class MonadMoment m => Testable m where
  interpretEvents :: (Event a -> m (Event b)) -> [Maybe a] -> IO [Maybe b]

instance Testable Moment where
  interpretEvents = interpret

instance Testable MomentIO where
  interpretEvents = interpretFrameworks

test_1_3_counter :: [Maybe ()] -> IO [Maybe Int]
test_1_3_counter =
  interpretEvents (counter :: Event () -> Moment (Event Int))

test_1_3_combined :: [Maybe ()] -> IO [Maybe String]

test_1_3_combined =
  interpretEvents (combined :: Event () -> Moment (Event String))

networkDescription :: EventSource () -> MomentIO ()
networkDescription t = do
  eTick <- fromAddHandler . addHandler $ t

  eCount <- counter eTick
  let eWrite = importantWork eCount

  reactimate $ (\x -> putStrLn $ "count " ++ show x) <$> eCount
  reactimate $ putStrLn <$> eWrite

eventLoop :: EventSource () -> IO ()
eventLoop e =
  forever $ do
    threadDelay 1000000
    fire e ()

go_1_3 :: IO ()
go_1_3 = do
  input <- mkEventSource
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
