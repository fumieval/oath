{-# LANGUAGE RankNTypes #-}
import qualified Oath as O
import qualified Control.Concurrent.Async as A
import Test.Tasty.Bench
import qualified Streamly.Prelude as S

main :: IO ()
main = defaultMain
  [ bench "oath 10" $ nfIO $ O.evalOath $ traverse (O.oath . pure) [0 :: Int ..9]
  , bench "async 10" $ nfIO $ A.runConcurrently $ traverse (A.Concurrently . pure) [0 :: Int ..9]
  , bench "streamly 10" $ nfIO $ S.drain $ S.fromZipAsync $ traverse (S.fromEffect . pure) [0 :: Int ..9]
  , bench "oath 100" $ nfIO $ O.evalOath $ traverse (O.oath . pure) [0 :: Int ..99]
  , bench "async 100" $ nfIO $ A.runConcurrently $ traverse (A.Concurrently . pure) [0 :: Int ..99]
  , bench "streamly 100" $ nfIO $ S.toList $ S.fromZipAsync $ traverse (S.fromEffect . pure) [0 :: Int ..99]
  ]
