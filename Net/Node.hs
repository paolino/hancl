-- | Node is the structure describing the links of a value.It keeps them ordered by the distance between the value and its linked companion
module Net.Node where

import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Control.Arrow
import Datas
import Lib.Shuffle
import System.Random
import Control.Monad.State

-- | Main datatype
data (Metrics s) => Node s = Node { -- | Value of the node 
                                    value :: s ,                
                                    -- |  Sorted by distance links
                                    sorteds :: S.Set (Float,Int), 
                                    -- | Links database, key is the node index in the driving structure, 
                                    --  value  is the couple distance, untouched passes 
                                    db :: IM.IntMap (Float,Int)} 

instance (Metrics s,Show s) => Show (Node s) where
  show n = (show.IM.keys.db $ n) ++ " " ++ show (value n)

-- | Create an empty Node
empty :: (Metrics s) => s         -- ^ The value of the node
                        -> Node s -- ^ The node
empty s = Node s S.empty IM.empty
     
-- | Node mixer     
union :: (Metrics s) => Node s     -- ^ One Node
                        -> Node s  -- ^ One Node
                        -> Node s  -- ^ The mix of them
union (Node x s1 db1) (Node _ s2 db2) = Node x (S.fromList $ map (fst.snd &&& fst) $ IM.assocs db) db
  where db = IM.union db1 db2

-- | Core insertion 
inserter k dv n@(Node s sorteds db) =  dv `seq` k `seq` n {sorteds = S.insert (dv,k) sorteds ,db = (IM.insert k (dv,0) db) }  
-- | Core deager
deager  k  n@(Node s sorteds db) = k `seq` Node s sorteds (IM.adjust (\(v,_) -> (v,0)) k db)

-- | Reset the link age if the link is present, or add it by value
insert :: (Metrics s) => Int       -- ^ Index of the linked node
                         -> s      -- ^ Value of the linked node       
                         -> Node s -- ^ Node to be modified     
                         -> Node s -- ^ Modified node           
insert k v n@(Node s sorteds db) 
  | IM.member k db     = deager k n 
  | otherwise          = inserter k (value n -| v) n

-- | Reset the link age if the link is present, or add it by distance
insert' :: (Metrics s) => Int       -- ^ Index of the linked node
                         -> Float  -- ^ Distance between       
                         -> Node s -- ^ Node to be modified     
                         -> Node s -- ^ Modified node           
insert' k dv n@(Node s sorteds db) 
  | IM.member k db     = deager k n
  | otherwise          = inserter k dv n

-- | Plural version of insert'
inserts' :: (Metrics s) => [(Int, Float)] -> Node s -> Node s
inserts' = flip $ foldl (\fs (k,v) -> insert' k v fs) 

-- | Plural version of insert
inserts :: (Metrics s) => [(Int, s)] -> Node s -> Node s
inserts  = flip $ foldl (\fs (k,v) -> insert k v fs) 

-- | Create a node given the value and a list of couple index,value of the linkeds
create :: (Metrics s) => s              -- ^ The value of the node
                         -> [(Int, s)]  -- ^ List of links as index,value couple
                         -> Node s      -- ^ Resulting Node
create s kvs = inserts kvs $ empty s 

-- | Links extractor
links :: (Metrics s) => Node s             -- ^ A node
                        -> [(Int, Float)]  -- ^ Ordered (by distance) links as index,distance
links = map (snd &&& fst). S.toList. sorteds

-- | Add one to the age of every link
age :: (Metrics s) => Node s     -- ^ A node 
                      -> Node s  -- ^ Node with aged links   
age fs@(Node _ _ db) = fs {db = IM.map (id *** (+1)) db}

-- | Old links cutter
cut :: (Metrics s) => Int        -- ^ Cutting age  (older links  will be deleted)     
                      -> Node s  -- ^ A node
                      -> Node s  -- ^ Cutted node
cut n fs@(Node _ sorteds db) = fs { sorteds = S.filter (flip IS.member (IM.keysSet db'). snd) sorteds, db = db'} 
  where db' = IM.filter ((<n).snd) db

deviation :: (Metrics s) => (Int -> Node s) -> (Node s) -> Float
deviation  rf n =
  uncurry (/).(id *** fromIntegral) $ IM.foldWithKey (\k _ (s,c) -> (s + (rf' k -| value n),c+1)) (0,0) (db n)
    where rf' = value.rf

-- | number of links of a Node
size :: (Metrics s) => Node s  -- a Node
                       -> Int  -- number of links from it to the rest
size = S.size.sorteds
  
