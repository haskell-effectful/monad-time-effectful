{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Time measurement via 'MonadTime'.
module Effectful.Time
  ( -- * Effect
    Time (..)
  , MonadTime (..)

    -- ** Handlers
  , runTime
  , runFrozenTime
  , runFixedStepTime
  ) where

import Control.Monad.IO.Class
import Control.Monad.Time
import Data.Time
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import GHC.Clock (getMonotonicTime)

-- | Provide the ability to use the 'MonadTime' instance of 'Eff'.
data Time :: Effect where
  CurrentTime :: Time m UTCTime
  MonotonicTime :: Time m Double

type instance DispatchOf Time = Dynamic

-- | Run a 'Time' effect via 'IO'.
runTime :: IOE :> es => Eff (Time : es) a -> Eff es a
runTime = interpret $ \_ -> \case
  CurrentTime -> liftIO getCurrentTime
  MonotonicTime -> liftIO getMonotonicTime

-- | Run a 'Time' effect with a frozen value of the 'CurrentTime' operation.
--
-- /Note:/ the 'MonotonicTime' operation works the same way as in 'runTime'.
runFrozenTime :: IOE :> es => UTCTime -> Eff (Time : es) a -> Eff es a
runFrozenTime time = interpret $ \_ -> \case
  CurrentTime -> pure time
  MonotonicTime -> liftIO getMonotonicTime

-- | Run the 'Time' effect with a given starting time; time advances
-- by a fixed increment for every invocation of the 'CurrentTime'
-- operation. A negative increment will make the clock run backwards.
--
-- /Note:/ the 'MonotonicTime' operation works the same way as in
-- 'runTime'.
runFixedStepTime :: IOE :> es => UTCTime -> NominalDiffTime -> Eff (Time : es) a -> Eff es a
runFixedStepTime start diff =
  reinterpret_ (evalState start) $ \case
    CurrentTime -> do
      state $ \s -> (s, diff `addUTCTime` s)
    MonotonicTime -> liftIO getMonotonicTime

----------------------------------------
-- Orphan instance

instance Time :> es => MonadTime (Eff es) where
  currentTime = send CurrentTime
  monotonicTime = send MonotonicTime
