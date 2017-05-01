# outer

[![Build Status](https://travis-ci.org/rob-b/outer.png)](https://travis-ci.org/rob-b/outer)

Wrapper around `ghc-mod legacy-interactive`.

Currently missing a client and so not very useful.

```sh
> outer
Listening socket ready...
```

And then in another term:

```sh
> nc 127.0.0.1 8080
check src/Outer.hs
src/Outer.hs:71:8:Warning: A do-notation statement discarded a result of type Int
Suppress this warning by saying _ <- sendAll p msg' msgNoSignal
src/Outer.hs:196:3:Warning: A do-notation statement discarded a result of type
  GHC.Conc.Sync.ThreadId
Suppress this warning by saying
  _ <- ($) forkIO talk inChan outChan
OK
```

