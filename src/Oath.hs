{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Oath
  ( Oath(..)
  , hoistOath
  , evalOathSTM
  , evalOathIO
  , oathSTM
  , oathIO
  , delaySTM) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import Control.Exception

-- 'Oath' is an 'Applicative' structure that collects results of one or more computations.
newtype Oath m a = Oath { runOath :: forall r. (m a -> IO r) -> IO r }
  deriving Functor

hoistOath :: (forall x. m x -> n x) -> Oath m a -> Oath n a
hoistOath t (Oath m) = Oath $ \cont -> m $ cont . t

evalOathSTM :: Oath STM a -> IO a
evalOathSTM m = runOath m atomically

evalOathIO :: Oath IO a -> IO a
evalOathIO m = runOath m id

instance Applicative m => Applicative (Oath m) where
  pure a = Oath $ \cont -> cont (pure a)
  Oath m <*> Oath n = Oath $ \cont -> m $ \f -> n $ \x -> cont (f <*> x)

instance Alternative m => Alternative (Oath m) where
  empty = Oath $ \cont -> cont empty
  Oath m <|> Oath n = Oath $ \cont -> m $ \a -> n $ \b -> cont (a <|> b)

-- | Lift an IO action into an 'Oath', forking a thread.
-- When the continuation terminates, it kills the thread.
-- Exception thrown in the thread will be propagated to the result.
oathSTM :: IO a -> Oath STM a
oathSTM act = Oath $ \cont -> do
  v <- newEmptyTMVarIO
  tid <- forkFinally act (atomically . putTMVar v)
  let await = readTMVar v >>= either throwSTM pure
  cont await `finally` killThread tid

-- | Like 'oathSTM', but the result is 'IO'
oathIO :: IO a -> Oath IO a
oathIO act = Oath $ \cont -> do
  v <- newEmptyMVar
  tid <- forkFinally act (putMVar v)
  let await = readMVar v >>= either throwIO pure
  cont await `finally` killThread tid

-- | An 'Oath' that finishes once the given number of microseconds elapses
delaySTM :: Int -> Oath STM ()
delaySTM dur = Oath $ \cont -> bracket (newDelay dur) cancelDelay (cont . waitDelay)