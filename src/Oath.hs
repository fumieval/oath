{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
module Oath
  ( Oath(..)
  , hoistOath
  , evalOath
  , oath
  , delay
  , timeout) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import Control.Exception
import Data.Monoid

-- 'Oath' is an 'Applicative' structure that collects results of one or more computations.
newtype Oath a = Oath { runOath :: forall r. (STM a -> IO r) -> IO r }
  deriving Functor
  deriving (Semigroup, Monoid) via Ap Oath a

hoistOath :: (forall x. STM x -> STM x) -> Oath a -> Oath a
hoistOath t (Oath m) = Oath $ \cont -> m $ cont . t

evalOath :: Oath a -> IO a
evalOath m = runOath m atomically

instance Applicative Oath where
  pure a = Oath $ \cont -> cont (pure a)
  Oath m <*> Oath n = Oath $ \cont -> m $ \f -> n $ \x -> cont (f <*> x)

instance Alternative Oath where
  empty = Oath $ \cont -> cont empty
  Oath m <|> Oath n = Oath $ \cont -> m $ \a -> n $ \b -> cont (a <|> b)

-- | Lift an IO action into an 'Oath', forking a thread.
-- When the continuation terminates, it kills the thread.
-- Exception thrown in the thread will be propagated to the result.
oath :: IO a -> Oath a
oath act = Oath $ \cont -> do
  v <- newEmptyTMVarIO
  tid <- forkFinally act (atomically . putTMVar v)
  let await = readTMVar v >>= either throwSTM pure
  cont await `finally` killThread tid

-- | An 'Oath' that finishes once the given number of microseconds elapses
delay :: Int -> Oath ()
delay dur = Oath $ \cont -> bracket (newDelay dur) cancelDelay (cont . waitDelay)

-- | Wrap an 'Oath'
timeout :: Int -> Oath a -> Oath (Maybe a)
timeout dur m = Just <$> m <|> Nothing <$ delay dur