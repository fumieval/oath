{-# LANGUAGE RankNTypes #-}
import qualified Oath as O
import qualified Control.Concurrent.Async as A
import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bench "oath 10" $ nfIO $ O.evalOath $ traverse (O.oath . pure) [0 :: Int ..9]
  , bench "async 10" $ nfIO $ A.runConcurrently $ traverse (A.Concurrently . pure) [0 :: Int ..9]
  , bench "oath 100" $ nfIO $ O.evalOath $ traverse (O.oath . pure) [0 :: Int ..99]
  , bench "async 100" $ nfIO $ A.runConcurrently $ traverse (A.Concurrently . pure) [0 :: Int ..99]
  ]
