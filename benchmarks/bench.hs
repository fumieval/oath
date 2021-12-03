{-# LANGUAGE RankNTypes #-}
import qualified Oath as O
import qualified Control.Concurrent.Async as A
import Test.Tasty.Bench

main = defaultMain
  [ bench "oath IO 10" $ nfIO $ O.evalOathIO $ traverse (O.oathIO . pure) [0 :: Int ..9]
  , bench "oath STM 10" $ nfIO $ O.evalOathSTM $ traverse (O.oathSTM . pure) [0 :: Int ..9]
  , bench "async 10" $ nfIO $ A.runConcurrently $ traverse (A.Concurrently . pure) [0 :: Int ..9]
  , bench "oath IO 100" $ nfIO $ O.evalOathIO $ traverse (O.oathIO . pure) [0 :: Int ..99]
  , bench "oath STM 100" $ nfIO $ O.evalOathSTM $ traverse (O.oathSTM . pure) [0 :: Int ..99]
  , bench "async 100" $ nfIO $ A.runConcurrently $ traverse (A.Concurrently . pure) [0 :: Int ..99]
  ]
