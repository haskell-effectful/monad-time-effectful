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
  ) where

import Control.Monad.IO.Class
import Control.Monad.Time
import Data.Time
import Effectful
import Effectful.Dispatch.Dynamic
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

----------------------------------------
-- Orphan instance

instance Time :> es => MonadTime (Eff es) where
  currentTime = send CurrentTime
  monotonicTime = send MonotonicTime
