module Cluster.Core  where

import Data.List (sortBy)
import Data.Array.Diff
import Control.Arrow 
import Control.Monad.Cont

import Datas 
import Lib.Matrix

inc l i = l // [(i,l!i + 1)]
switch l i = l // [(i,not (l!i))]
append l i e  = l // [(i, e : l!i)]

type Sorteds = [(Int,Int)]
type Clusters a = [[a]]

data F = F{cs::DA [Int],count::Int,rest::Int,ccounts::DA Int,rflags::DA Bool}
placer :: (Int,Int) -> Sorteds -> Clusters Int
-- placer (npoints,nclusters) orderedCouples -> clustered points
placer (lr,lc) ls = elems . cs . evalCC $ devilC where
  evalCC = flip runCont id . callCC
  devilC exit = foldM devil (F (kDA lc []) 0 r (kDA lc 0) (kDA lr False)) ls          
    where (k,r) = divMod lr lc  
          devil s@(F cs n r hs fs) (row, col) 
            | n >= lr  = exit s -- opla, job finished before list folded
            | otherwise = return $ if (not (fs ! row)) then rowtest else s            
                where ns = F (append cs col row) (n+1) r (inc hs col) (switch fs row)
                      count = hs ! col                                                                          
                      rowtest  | count < k               =  ns
                               | (count == k) && (r > 0) =  ns {rest = r -1}
                               | otherwise               =  s                                                      
        
type Dims = (Int,Int)
type Sorting = DM Float

sort :: Sorting -> Sorteds
sort matrix = sortBy (\x y -> compare (matrix!x) (matrix!y)) $ indices matrix        

boot :: (Metrics a) => Points a -> Points a -> (Sorting,Sorting -> Clusters a)
boot ps cs  = (lDM dims [(-|) p c | p <- ps, c <- cs],map (map (lA rc ps !)) . placer dims . sort)
   where dims@(rc,lc) = (length ps,length cs) 

        
        


