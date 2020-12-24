{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS
  -XMultiParamTypeClasses
  -XFlexibleContexts
#-}

module Memo where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

newtype Memo k v a = Memo (State (M.Map k v) a) deriving (Functor, Applicative, Monad)

unMemo :: Memo k v a -> State (M.Map k v) a
unMemo (Memo s) = s

startEvalMemo :: Memo k v a -> a
startEvalMemo = flip evalState M.empty . unMemo

startRunMemo :: Memo k v a -> (a, M.Map k v)
startRunMemo = flip runState M.empty . unMemo

memo :: (Ord k) => (k -> Memo k v v) -> k -> Memo k v v
memo f x = do
  m <- Memo get
  case M.lookup x m of
    Just y -> return y
    Nothing -> 
        do
          y <- f x
          Memo (modify (M.insert x y))
          return y

fib :: Int -> Memo Int Int Int
fib 0 = return 0
fib 1 = return 1
fib n = do
  a <- memo fib (n-1)
  b <- memo fib (n-2)
  return (a+b)

ans = startRunMemo $ fib 20

{-as an extra step you could have it check whether n is already in, like this
fib' g 0 = 0
fib' g 1 = 1
fib' g n = do
  a <- memo g (n-1)
  b <- memo g (n-2)

fib n = do
  m <- lift get
  case M.lookup x m of
    Just y -> return y
    Nothing -> fib' fib n

but this seems redundant.
-}
