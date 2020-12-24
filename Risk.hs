{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS
  -XMultiParamTypeClasses
  -XFlexibleContexts
#-}

module Risk where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Array as A
import Data.Ord
import Memo
import Data.Maybe

type I = Int
type F = Float

probs :: M.Map (I,I)  [F]
probs = M.fromList [((3,2),[2275/7776, 2611/7776, 2890/7776]), ((3,1),[441/1296,855/1296]), ((2,2),[581/1296,420/1296,295/1296]), ((2,1),[91/216,125/216]), ((1,2),[161/216,55/216]),((1,1),[21/36, 15/36])]
-- https://colab.research.google.com/drive/11O1yN8GcXhk-97piyxNeGQmK199RuMb2#scrollTo=bk7-nuPEy8rw

getCase :: (I, I) -> (I, I)
getCase (x,y) = ((min 4 x)-1, min 2 y)
-- (clamp 2 4 x)-1, clamp 1 2 y)

-- add a list of maybes...
sumMaybes :: (Num a) => [Maybe a] -> a
sumMaybes = sum . map (fromMaybe 0)

maybeGet :: (A.Ix a) => a -> A.Array a b -> Maybe b
maybeGet i arr = let
    (lo,hi) = A.bounds arr
    in
      if i>=lo && i<=hi then Just $ arr A.! i else Nothing

-- make arrays into a module...
-- can be more general...
linComb :: [Float] -> [A.Array I F] -> A.Array I F
linComb coeffs as = let
    boundsList = map A.bounds as
    amin = minimum $ map fst boundsList
    amax = maximum $ map snd boundsList
    indmap i = sumMaybes $ zipWith (\coeff ma -> fmap (coeff*) ma) coeffs (map (maybeGet i) as) 
    in A.listArray (amin, amax) $ map indmap [amin..amax]

risk :: (I, I) -> Memo (I,I) (A.Array I F) (A.Array I F)
risk (atk, 0) = return ((A.listArray (0,atk) (repeat 0)) A.// [(atk,1.0)])
risk (1, def) = return ((A.listArray (-def,0) (repeat 0)) A.// [(-def,1.0)])
--risk (1,1) = return (A.listArray (0,0) [1.0])
risk (atk, def) = do
  let (x,y) = getCase (atk, def)
  let m = min x y
  let ps = probs M.! (x,y) --[F]
  tables <- mapM (\i -> memo risk (atk-m+i, def-i)) [0..((length ps) - 1)]
  return $ linComb ps tables 
  -- now multiply and add these tables...

calcRisk :: (I,I) -> A.Array I F
calcRisk = startEvalMemo . risk

printRisk :: (I,I) -> IO ()
printRisk (a,d) = do
  let res = calcRisk (a,d)
  printArray res
  let (_,m) = A.bounds res
  let winProb = sum $ map (res A.!) [1..m]
  let loseProb = 1-winProb
  putStrLn $ "win\t"++(show winProb)
  putStrLn $ "lose\t"++(show loseProb)

printArray :: (A.Ix k, Show k, Show v) => A.Array k v -> IO ()
printArray arr = do
  let is = A.indices arr
  mapM_ (\i -> putStrLn $ (show i)++"\t"++(show $ arr A.! i)) is

-- ans = startRunMemo $ risk (10,5)

{-
clamp :: (Ord a) => a -> a -> a -> a
clamp lo hi x = if x<lo then lo 
                else if x>hi then hi 
                     else x
-}
