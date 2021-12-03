Oath: composable concurrent computation done right
----

Oath is an interface that makes concurrent actions composable.

```haskell
newtype Oath m a = Oath { runOath :: forall r. (m a -> IO r) -> IO r }
```

`Oath` is a continuation-passing IO action which takes an action to obtain the final result (`m a`).
The continuation-passing style makes it easier to release resources in time.
The easiest way to construct `Oath` is `oathSTM`. It run the supplied action in a separate thread as long as the continuation is running.

```haskell
oathSTM :: IO a -> Oath STM a
oathSTM act = Oath $ \cont -> do
  v <- newEmptyTMVarIO
  tid <- forkFinally act (atomically . putTMVar v)
  let await = takeTMVar v >>= either throwSTM pure
  cont await `finally` killThread tid
```

`Oath` is an `Applicative`, so you can combine multiple `Oath`s. It starts computations without waiting for the results. Run `evalOathSTM` to get the final result:

```haskell
evalOathSTM :: Oath STM a -> IO a
evalOathSTM m = runOath m atomically
```

It _does not_ provide a Monad instance because it is logically impossible to define one consistent with the Applicative instance.

Usage
----

`Oath` abstracts a triple of sending a request, waiting for response, and cancelling a request. If you want to send requests in a deterministic order, you can construct `Oath` directly instead of calling `oathSTM`.

```haskell
Oath $ \cont -> bracket sendRequest cancelRequest (cont . waitForResponse)
```

Timeout behaviour can be easily added using the `Alternative` instance and `delaySTM`. Or more in general, `<|>` expresses race

```haskell
-- | An 'Oath' that finishes once the given number of microseconds elapses
delaySTM :: Int -> Oath STM ()

oathSTM action <|> delaySTM 100000
```

Comparison to other packages
----

[future](https://hackage.haskell.org/package/future-2.0.0/docs/Control-Concurrent-Future.html), [caf](https://hackage.haskell.org/package/caf-0.0.3/docs/Control-Concurrent-Futures.html) and [async](https://hackage.haskell.org/package/async-2.2.4/docs/Control-Concurrent-Async.html) seem solve the same problem. They define abstractions to asynchronous computations. `async` has an applicative `Concurrently` wrapper which forks threads twice the number of terms for `(<*>)` and `(<|>)`. `async` is the most maintained.

[spawn](https://hackage.haskell.org/package/spawn-0.3/docs/Control-Concurrent-Spawn.html) does not define any datatype and provides an utility function for `IO`. It does not offer a way to cancel a computation.

[promises](https://hackage.haskell.org/package/promises-0.3/docs/Data-Promise.html) provides a monadic interface for pure demand-driven computation. It has nothing to do with concurrency.

[unsafe-promises](https://hackage.haskell.org/package/unsafe-promises-0.0.1.3/docs/Control-Concurrent-Promise-Unsafe.html) creates an IO action that waits for the result on-demand using `unsafeInterleaveIO`.

[futures](https://hackage.haskell.org/package/futures-0.1/docs/Futures.html) provides a wrapper of `forkIO`. There is no way to terminate an action and it does not propagate exceptions.

[promise](https://hackage.haskell.org/package/promise-0.1.0.0/docs/Control-Concurrent-Promise.html) has illegal Applicative and Monad instances; `(<*>)` is not associative and has a bind that's not consistent with `(<*>).`
