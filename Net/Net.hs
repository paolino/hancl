module Net.Net (Net,Memory,Collect(..),Tag(..),Refine(..),Schedule (..),nodes,schedulings,
                ecollect, etag, values, node, Net.Net.create,unNodes,Env,
                evolve, evolution,run, Net.Net.run0, Nodes, mean,NetState) where

import Data.List (sortBy,nub)
import System.Random
import Control.Monad.State
import Control.Monad.Reader
--import Control.Monad.RWS
import Control.Arrow
import Data.Array.Diff
import Data.Array.ST

import Net.Worker
import Control.Monad.ST
import Lib.Shuffle  as Shuffle
import Datas
import Lib.Matrix
import Net.Node  as Node
----------------------------  Classes ---------------------------------------------------------


-- | Collect abstract the way of picking new links among linkeds of linked.
class   Collect r where
  collect :: (Metrics s, Schedule h) => Int                    -- ^ The index of the node to give new links
                                        -> Net s h r q e w [Int]   -- ^ A list of valid node indexes

-- | Tagging links for random deletions  1 -> sure deletion ... 0 -> sure non deletion
class Tag q where
  tag :: q          -- ^ A reader argument identifying the tagger
         -> [Float] -- ^ The list of distances between the node and its friends
         -> [Float] -- ^ A congruent list of numbers with range (0,1)

-- | Refining core procedure for a Node
class Refine e where
  refine :: (Metrics s, Schedule h,Collect r,Tag q) =>
    Int                                    -- ^ Index of the node to be refined
    -> Net s h r q e w [(Int,Float)]     -- ^ Nodes to be notified as linked coupled with their distance

-- | Scheduling procedures to concentrate computation on hot spots smaller values of h                       
class (Random h, Ord h, Num h) => Schedule h  where
  zero      ::  h                 -- ^ Zero
  schedule   ::  (Metrics s) => Node s                -- ^ Node before evolution
                 -> Node s             -- ^ Node after evolution
                 -> h                  -- ^ Scheduling value greater than zero
  mediate    ::  Float                 -- ^ Dissipation constant
                 -> [h]                -- ^ Scheduling values to be mediate (the cluster)
                 -> [h]                -- ^ Mediated scheduling values

-----------------------------  Types and Datas ----------------------------------------------------

newtype (Metrics s) => Nodes s = Nodes {unNodes :: A (Node s)}

newtype  (Schedule h) => Schedulings h = Schedulings {unSchedulings :: A h} deriving Show

data (Metrics s,Schedule h) => NetState s h = NetState {nodes::Nodes s,schedulings::Schedulings h }


instance (Metrics s,Show s) => Show (Nodes s) where
  show (Nodes ns) = foldr (++) "" $ map ((++"\n").show) $ map resolve $ elems ns
    where  resolve n = (value n,map (value.(ns!).fst) $ links n)

-- | Memory hold the number of cycles before an untouched link is deleted
type Memory = Int

-- | Temperature memory

-- | We let three interfaces intervein
data Env r q e = Env {memory :: Memory, dissipation :: Float, ecollect :: r, etag :: q, erefiner :: e}

-- | Net is the main type hiding a mutable state of Nodes , an environment representing the kind of picking of
-- | new friends population and a stateful random generator
type Net s h r q e w = Worker (Env r q e) (NetState s h) w

------------- functions docs and types------------------------------------------------------

-- | measure the mean of the nodes deviation
mean :: (Metrics s, Schedule h) => Net s h r q e w (Float)

-- | Extract the values of the indexed nodes
values :: (Metrics s, Schedule h) => [Int]              -- ^ Indexes of nodes to be read
                         -> Net s h r q e w [s] -- ^ List of relative values

-- | Read the node at a given index
node :: (Metrics s, Schedule h) => Int                      -- ^ Index of the node to read
                       -> Net s h r q e w (Node s)  -- ^ Node from index

-- | Creation function from the starting number wanted of links per node and the list of values
create :: (Metrics a) => Int        -- ^ Initial number of links per node
                         -> [a]     -- ^ List of values to be clustered
                         -> Nodes a -- ^ Structure holding the net

-- | Update a node via higher order
updateNode :: (Metrics s, Schedule h) => (Node s -> Node s) -- ^ Node modifier
                         -> Int             -- ^ Node Index
                         -> Net s h r q e w ()  -- ^ Void output

-- | kill old links
killold :: (Metrics s, Schedule h) => Int    -- ^ the node to be lighten
          -> Net s h  r q e w ()  -- resulting net

-- | refresh some links
refresh :: (Metrics s, Schedule h) => Int            -- ^ index of a Node
           -> [(Int,Float)]  -- ^ (index,distance) list of linked nodes
           -> Net s h r q e w () -- ^ refreshed Net

-- | Refine the all array of nodes calling refine method specified in Engine class
evolve :: (Ord h,Metrics s, Schedule h, Collect r,Tag q,Refine e) => Int -> Net s h r q e w () -- ^ Void output

{-

-- | Run function for a Net action, random generator agnostic
run :: (Collect r,Tag q,Metrics s, Schedule h) =>
       h                          -- ^ Starting schedule value
       -> Memory                        -- ^ The number of passes a link resist in a node structure without being touched
       -> Float
       -> r                          -- ^ A value of a type implementing class Collect
       -> q                          -- ^ A value of a type implementing class Tag
       -> e                          -- ^ A value of a type implementing class Refine
       -> Nodes s                    -- ^ Starting Nodes structure
       -> Net s h r q e w a              -- ^ A valid action for the Net monad
       -> State g (a, Nodes s, w)       -- ^ The result of the action coupled with the state of the net under the state of a RNG

-- | Run function for a Net action , running with a stdgen with seed 0
run0 :: (Collect r, Tag q, Metrics s, Schedule h) =>
        h                                  -- ^ Starting schedule value
        -> Memory                                -- ^ The number of passes a link resist in a node structure without being touched
        -> Float
        -> r                                  -- ^ A value of a type implementing class Collect
        -> q                                  -- ^ A value of a type implementing class Tag
        -> e                          -- ^ A value of a type implementing class Refine
        -> Nodes s                            -- ^ Starting Nodes structure
        -> Net s h StdGen r q e w a   -- ^ A valid action for the Net monad
        -> (a, Nodes s,w)                       -- ^ The result of the action coupled with the state of the net

-}
-----------------------------------------------------------------------------------------------------------------------
    

mkstate ::(Metrics s , Schedule h) =>  Nodes s -> h -> NetState s h
mkstate nodes' peace = NetState 
  {nodes = nodes',
   schedulings = Schedulings $ lA (uncurry (flip (-)) (bounds array) + 1) (repeat peace)}
     where array = unNodes nodes'

mean = gets $ mean'. nodes
    where mean' (Nodes ns) = sum (map (Node.deviation (ns!)) $ elems ns) / fromIntegral (b-a+1)
             where (a,b) = bounds ns

values is = do sps <- gets $ (!).unNodes.nodes
               return $ map (value.sps) is                                

node i = gets $ (!i).unNodes.nodes
scheduling i = gets $ (!i).unSchedulings.schedulings

create n xs = Nodes (lA z (zipWith Node.create xs (map is [1..])))
  where is i = take n. drop i $ cycle (zip [1 .. z] xs)
        z = length xs

updateNode f i  = modify (\state @ NetState {nodes = Nodes ps} -> state{nodes = Nodes $  ps // [(i,f $ ps ! i)]})

updateScheduling f i = 
  modify (\state @ NetState {schedulings = Schedulings ps} -> state{schedulings = Schedulings $ ps // [(i,f $ ps ! i)]})

killold i =  do n <- (asks $ (\m -> age.cut m).memory) `ap` node i
                updateNode (const n) i

refresh i us = do  updateNode (inserts' us ) i                        -- touch node side of links
                   mapM_ (\(j,dv) -> updateNode (insert' i dv) j) us  -- touch linkeds side of links

--evolveS :: (Refine e, Tag q, Collect r, RandomGen g,Schedule h,Metrics s,Ord h ) => Net s h g r q e w ()
evolveS = do    ss <-  gets $ elems.unSchedulings.schedulings
                i <-  runrand $ (pick (zip ss [1..]) (zero, sum ss) :: (RandomGen g) => State g Int)
                n <- node i   -- the node before evolution             
                killold i 
                us <- refine i 
                refresh i us  -- evolution step                        
                n' <-  node i                          -- node after evolution                  
                let ls = map fst. links $ n                                                    
                ss <- liftM (schedule n n' : ) $ mapM scheduling ls -- linked nodes values     
                s:cs <-  liftM (flip mediate ss) $ asks dissipation  -- the scheduling values   
                mapM_ (\(s,l) -> updateScheduling (const s) l) $ zip cs ls                     
                updateScheduling (const s) i

evolve ns = replicateM_ ns evolveS

run generator operation nodes' memory dissipation peace ecollect etag erefiner = 
  let Result a state log g =
        runWorker operation (Env memory dissipation ecollect etag erefiner) (mkstate nodes' peace) generator 
   in (nodes state,log)

run0 = run $ mkStdGen 0

logState = gets (\s -> (show.nodes $ s) ++ (show.schedulings $ s)) >>= tell
logMean = liftM show mean >>= tell


evolution ns convergence = do 
  evolve ns  
  m <- mean
  if m > convergence then 
     tell m >> evolution ns convergence 
     else return ()

                                   
