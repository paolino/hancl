module Lib.Distribution where

import Lib.Matrix
import Data.Map as Map
import Data.List

distribution ls = Map.toList $ foldl (\s x -> Map.insertWith (+) x 1 s) Map.empty ls

positions :: (Ord a) => [[a]] -> [(a,[(Int,Int)])]
positions = Map.toList . Map.map Map.toList . foldl positions' Map.empty 
  where positions' s ls = foldl adder s (zip [1..] ls) 
         where adder s (i,x) = Map.insertWith inside x first s
                where inside _ = Map.insertWith (+) i 1 
                      first = Map.insert i 1 Map.empty


distribution2 ls = let 
  parse stat = fst . foldl update (stat,1) where
    update (stat,n) x = (Map.insertWith (+) (x,n) 1 stat ,n+1)
  in sortBy (\x y -> compare (snd y) (snd x)) $ Map.toList $ foldl parse Map.empty ls










