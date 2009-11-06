-- | Classes and some instances for datas to be acceptable for clustering
module Datas where

import Data.List

-- | Minimum interface for datas. Implement a distance function.
class (Eq v,Show v) => Metrics v where
  (-|) :: v -> v -> Float

-- | Strong interface for datas. Implement average.
class Average v where
  average :: Points v -> v


type Points a = [a]

-- | Deviation of some datas from a pivot
deviation :: (Metrics a) => Points a  -- ^ List of datas
                            -> a      -- ^ The pivot
                            -> Float  -- ^ Deviation value
deviation [] _ = 0
deviation points pivot =  sum l / fromIntegral s where
  (s,l) = foldl (\(s,l) p -> (s + 1, pivot -| p : l)) (0,[]) points

-- | Mean deviation of some datas
meand :: (Metrics a,Average a) => Points a -- ^ List of datas
                                  -> Float -- ^ Mean deviation value
meand points = deviation points $ average points


instance Metrics Float where
  a -| b = abs (a - b)

instance Average Float where
  average ps = l/fromIntegral s  where
    (s,l) = foldl (\(n,l) x -> (n+1,l+x)) (0,0) ps

instance Metrics Int where
  (-|) a b = fromIntegral $ abs (a - b)

instance Average Int where
  average ps = floor $ (fromIntegral l)/fromIntegral s  where
    (s,l) = foldl (\(n,l) x -> (n+1,l+x)) (0,0) ps


-- | Levehenstein distance String type
newtype Lev = Lev String deriving (Eq,Read)
instance Show Lev where
  show (Lev x) = show x

instance Metrics Lev where
  (-|) (Lev ss) (Lev ts) = fst . last . foldl' nextRow (zip [0..] (' ':ss)) $ zip [1..] ts -- folds on the first column
     where nextRow ss' (n,t) = let r = (n,' '): zipWith3 takemin r ss' (tail ss') -- build the current row 
                                   takemin (pr,_) (pc,_) (cc,s) =
                                      let cost = if s == t then 0 else 1         -- comparing each element
                                      in (minimum [pr+1,pc+cost,cc+1],s)
                               in r



data Data' = DataInt Int| DataLev Lev deriving (Read,Show)


unData :: [Data'] -> (forall a.Metrics a => [a] -> ret) -> ret
unData ds@(DataInt _:_) fn = fn (map unbox ds) where unbox (DataInt n) = n 
--unData ds@(DataFloat _:_) fn = fn (map unbox ds) where unbox (DataFloat n) = n 
unData ds@(DataLev _:_) fn = fn (map unbox ds) where unbox (DataLev n) = n 
