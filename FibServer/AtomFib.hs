module AtomEx2
  ( compileFib ) where

import Language.Atom

-- | Invoke the atom compiler.
compileFib :: IO ()
compileFib = do
  compile "fibDev" $ fibDev
  compile "fibDvr" $ fibDvr

-- Fibonacci device: takes an index x from an external client and returns
-- fib(x).
fibDev :: Atom ()
fibDev = period 3 $ do
  fst      <- word64 "fst" 1 -- fib(0)
  snd      <- word64 "snd" 1 -- fib(1)
  ans      <- word64 "ans" 0 -- answer returned
  -- external signals --
  x        <- word64' "x" -- index value from driver
  newInd   <- bool' "newInd" -- index from driver ready 
  ----------------------
  i        <- word64 "i" 0 -- local copy of received index
  ansReady <- bool "ansReady" False -- fib(i) computed?
  valRcvd  <- bool "valRcvd" False -- ack that the index is received
  runFib   <- bool "runFib" False -- computing fib(i)?

  -- wait for a new index from the client
  atom "getIndex" $ do
    cond $ not_ (value runFib)
    cond $ value newInd
    i        <== value x
    runFib   <== true
    fst      <== 1
    snd      <== 1
    ansReady <== false
    valRcvd  <== true

  -- generate fib(i)
  atom "computeFib" $ do
    cond $ value runFib
    cond $ value i >. 0
    decr i
    snd <== (value fst) + (value snd)
    fst <== value snd

  -- send fib(i) back to the client
  atom "sendVal" $ do
    cond $ value i ==. 0
    cond $ value runFib
    runFib   <== false
    ans      <== value fst
    ansReady <== true
    valRcvd  <== false

-- Fibonacci driver: generate a new index x and wait to receive fib(x).
fibDvr :: Atom ()
fibDvr = period 20 $ do
  x        <- word64 "x" 0 -- new index to send
  oldInd   <- word64 "oldInd" 0 -- previous index sent
  -- external signals --
  valRcvd  <- bool' "valRcvd" -- has the device received the new index?
  ans      <- word64' "ans" -- the newly-computed fib(x)
  ansReady <- bool' "ansReady" -- is an answer waiting?
  ----------------------
  valD     <- word64 "valD" 1 -- local copy of fib(x)
  newInd   <- bool "newInd" True -- a new index is ready
  waiting  <- bool "waiting" True -- waiting for a new computation

  atom "wait" $ do
    cond $ value valRcvd
    cond $ not_ $ value waiting
    newInd  <== false
    waiting <== true

  atom "getAns" $ do
    cond $ value ansReady
    cond $ value waiting
    cond $ value x <. 50
    valD    <== value ans
    x       <== value x + 5
    waiting <== false
    newInd  <== true
    oldInd  <== value x
