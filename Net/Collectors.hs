-- | Some valid Collector instances for Net type
module Net.Collectors where

import Net.Net
import Lib.Shuffle
import Net.Node
import Datas
import Net.Worker

import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import System.Random

data Collector = KPath' KPath | KRPath' KRPath | RandomCollector' RandomCollector deriving (Read)

unCollect :: Collector -> (forall  a. (Collect a) => a -> ret ) -> ret
unCollect (KRPath' x)          fn = fn x
unCollect (KPath' x)           fn = fn x
unCollect (RandomCollector' x) fn = fn x


-- pick a random links among node links
oneFriend :: (Metrics s,Schedule h) => Int -> Net s h r q e w Int
oneFriend i = do n <- node i
                 j <- runrand $ getRandomR (0,size n - 1)
                 return.fst $ links n !! j 

-- | Uses the wrapped (Int,Int) as (numbersOfPaths,length) .
-- Picks all the touched nodes indexes taking numbersOfPaths random paths
-- of length length

newtype KPath = KPath {unKPath :: (Int,Int)}  deriving (Read,Show)

instance Collect KPath where
  collect i = do (n,d) <- asks $ unKPath.ecollect
                 liftM concat $ replicateM n $ foldM pick [i] [1..d] 
                   where pick fs@(f:_) _ = liftM (:fs) $ oneFriend f


newtype KRPath = KRPath {unKRPath::[Int]}   deriving (Read,Show)
instance Collect KRPath where
  collect t = do
    ns <- asks $ unKRPath.ecollect
    let
      run _ [] = return $ []
      run i (n:ns)= do
        ys <- liftM (map fst.links) $ node i
        hs <- runrand $ sample n ys    --dangerous !!
        rss <- mapM (flip run ns) ys
        return $ hs ++ concat rss
    run t ns                       
 
newtype RandomCollector = RandomCollector {unRandomCollector::Int}  deriving (Read,Show)
instance Collect RandomCollector where
  collect i = do bs <- gets $ bounds.unNodes.nodes
                 n <- asks $ unRandomCollector.ecollect
                 ns <- runrand $ getRandomRs bs
                 return $ take n ns
                 
