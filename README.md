Oath: composable concurrent computation done right
----

Oath is an Applicative structures that makes concurrent actions composable.

```haskell
newtype Oath a = Oath { runOath :: forall r. (STM a -> IO r) -> IO r }
```

`Oath` is a continuation-passing IO action which takes a transaction to obtain the final result (`STM a`).
The continuation-passing style makes it easier to release resources in time.
The easiest way to construct `Oath` is `oath`. It runs the supplied IO action in a separate thread as long as the continuation is running.

```haskell
oath :: IO a -> Oath a
oath act = Oath $ \cont -> do
  v <- newEmptyTMVarIO
  tid <- forkFinally act (atomically . putTMVar v)
  let await = takeTMVar v >>= either throwSTM pure
  cont await `finally` killThread tid

evalOath :: Oath a -> IO a
evalOath m = runOath m atomically
```

`Oath` is an `Applicative`, so you can combine multiple `Oath`s using `<*>`. `Oath` combined this way kicks off computations without waiting for the results. The following code runs `foo :: IO a` and `bar :: IO b` concurrently, then applies `f` to these results.

```haskell
main = evalOath $ f <$> oath foo <*> oath bar
```

It _does not_ provide a Monad instance because it is logically impossible to define one consistent with the Applicative instance.

Usage
----

`Oath` abstracts a triple of sending a request, waiting for response, and cancelling a request. If you want to send requests in a deterministic order, you can construct `Oath` directly instead of calling `oath`.

```haskell
Oath $ \cont -> bracket sendRequest cancelRequest (cont . waitForResponse)
```

Timeout behaviour can be easily added using the `Alternative` instance and `delay :: Int -> Oath ()`. `a <|> b` runs both computations until one of them returns a result, then cancels the other.

```haskell
-- | An 'Oath' that finishes once the given number of microseconds elapses
delay :: Int -> Oath ()

oath action <|> delay 100000
```

Comparison to other packages
----

[future](https://hackage.haskell.org/package/future-2.0.0/docs/Control-Concurrent-Future.html), [caf](https://hackage.haskell.org/package/caf-0.0.3/docs/Control-Concurrent-Futures.html) and [async](https://hackage.haskell.org/package/async-2.2.4/docs/Control-Concurrent-Async.html) seem solve the same problem. They define abstractions to asynchronous computations. `async` has an applicative `Concurrently` wrapper.

[spawn](https://hackage.haskell.org/package/spawn-0.3/docs/Control-Concurrent-Spawn.html) does not define any datatype. Instead it provides an utility function for `IO` (`spawn :: IO a -> IO (IO a)`). It does not offer a way to cancel a computation.

[promises](https://hackage.haskell.org/package/promises-0.3/docs/Data-Promise.html) provides a monadic interface for pure demand-driven computation. It has nothing to do with concurrency.

[unsafe-promises](https://hackage.haskell.org/package/unsafe-promises-0.0.1.3/docs/Control-Concurrent-Promise-Unsafe.html) creates an IO action that waits for the result on-demand using `unsafeInterleaveIO`.

[futures](https://hackage.haskell.org/package/futures-0.1/docs/Futures.html) provides a wrapper of `forkIO`. There is no way to terminate an action and it does not propagate exceptions.

[promise](https://hackage.haskell.org/package/promise-0.1.0.0/docs/Control-Concurrent-Promise.html) has illegal Applicative and Monad instances; `(<*>)` is not associative and its `ap` is not consistent with `(<*>)`.

Performance
----

```haskell
bench "oath 10" $ nfIO $ O.evalOath $ traverse (O.oath . pure) [0 :: Int ..9]
bench "async 10" $ nfIO $ A.runConcurrently $ traverse (A.Concurrently . pure) [0 :: Int ..9]
```

`Oath`'s overhead of `(<*>)` is less than `Concurrently`. Unlike `Concurrently`, `<*>` itself does not fork threads.

```
All
  oath 10:   OK (1.63s)
    5.78 μs ± 265 ns
  async 10:  OK (0.21s)
    12.3 μs ± 767 ns
  oath 100:  OK (0.22s)
    52.6 μs ± 4.4 μs
  async 100: OK (0.23s)
    109  μs ± 8.4 μs
```
