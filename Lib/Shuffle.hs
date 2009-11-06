-- | Random functions working with a rng under the State monad
module Lib.Shuffle  where

import System.Random
import Control.Monad.State
import Data.Array.Diff
import Lib.Matrix
import Control.Arrow

-- | Pick some elements linear-randomly from a list
sample :: (MonadState g m,RandomGen g) => Int      -- ^ number of pickings
                                          -> [a]   -- ^ pool
                                          -> m [a] -- ^ picked elements
sample n xs = swapper xs (length xs,n)

-- | Produce a linear-shuffled list from the elements
shuffle :: (MonadState g m,RandomGen g) => [a]        -- ^ list to shuffle
                                           -> m [a]   -- ^ shuffled list 
shuffle xs = swapper xs (l,l) where l = length xs


swapper :: (MonadState g m,RandomGen g) => [a] -> (Int, Int) -> m [a]
swapper xs (l,n) = liftM (take n . elems) $ foldM randSwap (dA l xs) [1 .. n]
  where randSwap a u = liftM (swap a u) (getRandomR (u,l))

-- | State version of randomR 
getRandomR :: (MonadState g m,RandomGen g,Random a) => (a,a)  -- ^ bounds for the value
                                                       -> m a -- ^ picked value
getRandomR range = do
  (x,g) <- gets (randomR range)            
  put g
  return x                         

-- | State version of randomRs
getRandomRs :: (MonadState g m,RandomGen g,Random a) => (a,a)    -- ^ bounds for the value
                                                        -> m [a] -- ^ streams of picked values
getRandomRs range = do
  (g',g'') <- gets split
  put g''
  return $ randomRs range g'

-- | Stream of random numbers under normal distribution
getGaussRs :: (MonadState g m, Floating a, RandomGen g, Random a) =>
              a               -- ^ mean deviation
              -> a            -- ^ mean
              -> m [a]        -- ^ stream
getGaussRs k q = do 
  ab <- liftM (uncurry zip. (id &&& tail)) $ getRandomRs (0,1)
  return $ map (\(a,b) -> q+k*(sqrt $ (-2)*log a/a)* cos (2*pi*b)) ab

ifrand :: (Random l,Ord l,MonadState g m,RandomGen g) => l -> (l,l) -> m Bool 
ifrand x d = getRandomR d >>= return.(< x)

-- | A cutting tool. Give it a list of elements and a list of floats in range (0,1)
-- it will eliminate the values from the list when the same-index float is
-- inferior to a linear-randomly picked value in range (0,1)
judge :: (MonadState g m,RandomGen g) => [a]          -- ^ elements to be judged
                                         ->  [Float]  -- ^ cutting probabilities
                                         -> m [a]     -- ^ survivers
judge xs js = do rs <- getRandomRs (0,1::Float)
                 return $ foldr (\(x,c) ks -> if c then x:ks else ks) [] $ zip xs (zipWith (<) rs js)
  -- | Weighted random picking
pick :: (MonadState g m,RandomGen g,Ord z,Random z,Num z)  => [(z,a)]     -- ^ list of weighted elements (weight,elem)
                                               -> (z,z)    -- ^ picking range !!
                                               -> m a        -- ^ picked elements
pick xs = liftM (flip distpick xs). getRandomR  where                                          
  distpick x = snd. head. dropWhile ((< x). fst). scanl1 (\(s, _) (z, a) -> (s + z, a))

-- | A test runner for this module function 
run0 :: (MonadState g m, RandomGen g) => (m a -> StdGen -> b)  -- ^ the action evaluator of the monad used
                                         -> m a                -- ^ a monadic operation using rng as state
                                         -> b                  -- ^ the result of the action unwrapped from monad
run0 eval = flip eval $ mkStdGen 0

-- Thanks to glguy for suggestions on monadic treatment


