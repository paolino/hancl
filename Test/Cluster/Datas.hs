module Test.Cluster.Datas where

import Datas
import System.Random
import Test.QuickCheck
import Data.List


cond xs ys = (length ys > length xs && length xs > 0)
  where types = (xs::[Int],ys::[Int])        

-- check all elements in xs are in the exit clusters
prop_all f xs ys = cond xs ys  ==> (sort.concat $ f xs ys) == sort xs

-- check the max difference in clusters filling is 1 
prop_diff f xs ys = cond xs ys ==> (length . group . sort . map length $ (f xs ys)) <= 2
 

