import qualified Oath
import qualified Futures
import qualified Control.Concurrent.Promise.Unsafe as UP
import qualified Control.Concurrent.Promise as Promise
import qualified Control.Concurrent.Async as Async
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Data.Functor.Compose

action :: Int -> String -> Double -> IO ()
action order name dur = do
  -- delay a bit so that "Begin" gets printed in a consistent order
  threadDelay $ 1000 * 10 * order
  putStrLn $ "    Begin " <> name
  threadDelay (floor $ 1000000 * dur)
  putStrLn $ "    End " <> name
  `onException` putStrLn ("    Killed " <> name)

tester :: Applicative m => (m () -> IO ()) -> (IO () -> m ()) -> IO ()
tester run lift = do
  putStrLn "  Left:"
  run $ (lift (action 0 "foo" 0.1) *> lift (action 1 "bar" 0.3)) *> lift (action 2 "baz" 0.2)
  putStrLn "  Right:"
  run $ lift (action 0 "foo" 0.1) *> (lift (action 1 "bar" 0.3) *> lift (action 2 "baz" 0.2))

  putStrLn "  Left Error:"
  try (run $ lift (fail "Fail") *> lift (action 0 "success" 0.1)) >>= print'
  putStrLn "  Right Error:"
  try (run $ lift (action 0 "success" 0.1) *> lift (fail "Fail")) >>= print'
  where
    print' :: Either SomeException () -> IO ()
    print' = putStrLn . ("    "++) . show

main :: IO ()
main = do
  putStrLn "oath timeout:"
  Oath.evalOath $ Oath.oath (action 0 "delay" 1.0) <|> Oath.delay 200000
  threadDelay 10000

  putStrLn "oath: "
  tester Oath.evalOath Oath.oath
  putStrLn "async: "
  tester Async.runConcurrently Async.Concurrently
  putStrLn "futures:"
  tester ((>>=Futures.block) . getCompose) (Compose . Futures.fork)
  putStrLn "promise:"
  tester Promise.runPromise Promise.liftIO

  -- random
  putStrLn "unsafe-promises:"
  try (tester id UP.promise) >>= (print :: Either SomeException () -> IO ())

