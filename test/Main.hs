module Main where

import qualified Data.Time as T
import Effectful
import Test.Tasty
import Test.Tasty.HUnit
import qualified Utils as U

import Effectful.Time
import Effectful.State.Static.Local

main :: IO ()
main = defaultMain $ testGroup "effectful-time"
  [ testCase "IO Time handler & State" $ testIOTimeAndState =<< T.getCurrentTime
  , testCase "Pure Time handler & State" testPureTimeAndState
  ]


testIOTimeAndState :: UTCTime -> Assertion
testIOTimeAndState firstTime = runEff $ do
  result <- evalState firstTime -- The order in which thes two functions
            . runCurrentTimeIO  -- are composed does not matter. Swap them to try.
            $ storingTimeInState
  U.assertEqual "" firstTime result

storingTimeInState :: (Time :> es, State UTCTime :> es) => Eff es UTCTime
storingTimeInState = do
  firstTime <- get
  secondTime <- action
  if secondTime <= firstTime
  then put secondTime
  else put firstTime
  get

action :: (Time :> es) => Eff es UTCTime
action = do
  T.addUTCTime 100 <$> getCurrentTime

---

testPureTimeAndState :: Assertion
testPureTimeAndState = runEff $ do
  let time = read "2021-07-11 13:30:20 UTC" :: UTCTime
  result <- runCurrentTimePure time
            . evalState time
            $ usingStaticTime
  U.assertEqual "" True result 

usingStaticTime :: (Time :> es, State UTCTime :> es) => Eff es Bool
usingStaticTime = do
  t <- getCurrentTime
  t' <- get
  pure $ t == t'
