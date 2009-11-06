-- | Array library to use DiffArrays and Arrays with Int index ....
module Lib.Matrix where

import Data.List
import Data.Array.Diff

data Line = Row|Col

no Row = Col
no Col =Row

type DM s = DiffArray (Int,Int) s
type DA s = DiffArray Int s
type A s = Array Int s

-- | create a DiffArray indexed by an Int from 1 
dA :: Int            -- ^ dimension
      -> [a]         -- ^ initializing list
      -> DA a        -- ^ diffarray
dA d = listArray (1,d) :: [a] -> DA a

-- | create an Array indexed by an Int from 1
lA ::  Int            -- ^ dimension
       -> [a]         -- ^ initializing list
       -> A a         -- ^ array
lA d = listArray (1,d) ::  [a] -> A a

-- | higher order array creator
al :: (IArray t a) => 
                      (Int -> [a] -> t Int a)  -- ^ pass it a dA or lA
                      -> [a]                   -- ^ the list to initialize it
                      -> t Int a               -- ^ the array created
al kA ls =  kA (length ls) ls

-- | bidimentional diffarray
lDM :: (Int, Int)  -- ^ dimensions
       -> [a]      -- ^ creating list
       -> DM a     -- ^ the matrix
lDM (d1,d2) = listArray ((1,1),(d1,d2)) :: [a] -> DM a

-- | constant value diffarray
kDA :: Int -> a -> DA a
kDA n v = listArray (1,n) (repeat v)                                               

-- | value swapper 
swap :: (IArray b a) => (b Int a)     -- ^ an array
                        -> Int        -- ^ first index
                        -> Int        -- ^ second index
                        -> (b Int a)  -- modified array
swap a i j = a // [(j,a!i),(i,a!j)]

-- | List indices of rows or columns of a matrix
lindices ::  (IArray b a) =>  Line               -- ^ col row selector
                              -> (b (Int,Int) a) -- ^ a matrix
                              -> Int             -- ^ line number
                              -> [(Int,Int)]     -- ^ list of indices
lindices t m n = 
  case t of 
    Row -> [(i,n) | i <- [r0..r1]]
    Col -> [(n,i) | i <- [c0..c1]]
    where
  ((r0,c0),(r1,c1)) = bounds m
-- | Enumeration of indices of rows or columns in a matrix
gindices :: (IArray b a) => Line               -- ^ col row selector
                            -> (b (Int,Int) a) -- ^ a matrix
                            -> [Int]           -- ^ enumeration of indices
gindices t m =
  case t of 
    Row -> [r0..r1]
    Col -> [c0..c1]
    where
  ((r0,c0),(r1,c1)) = bounds m
  
-- | Read a line from a matrix
lget :: (IArray a e) => Line               -- ^ col row selector
                        -> a (Int, Int) e  -- ^ a matrix
                        -> Int             -- ^ number of the line
                        -> [e]             -- ^ line read
lget t m l  = map (m!) (lindices t m l)

-- | Put a line in a matrix
lput :: (IArray a b) => Line               -- ^ col row selector
                        -> a (Int, Int) b  -- ^ a matrix
                        -> Int             -- ^ number of the line
                        -> [b]             -- ^ elements to be inserted
                        -> a (Int, Int) b  -- ^ modified matrix
lput t m l xs = m // zip (lindices t m l) xs

-- | modify elements of given indices with e function e -> e
laccum :: (IArray a e, Ix b) => (e -> e)     -- ^ element modifier
                                 -> a b e    -- ^ an array
                                 -> [b]      -- ^ list of indices
                                 -> a b e    -- ^ modified array            

laccum f m ls = accum (\e _ -> f e) m (zip ls (repeat undefined))

-- | line modifier 
transform :: (IArray a e) => Line                -- ^ col row selector
                             -> (e -> e)         -- ^ element modifier 
                             -> a (Int, Int) e   -- ^ an array         
                             -> Int              -- ^ line number
                             -> a (Int, Int) e   -- ^ modified array   
transform t f m l  = laccum f m (lindices t m l)
-- | matrix transformer line by line
transform' :: (IArray a e) => Line                -- ^ col row selector 
                              -> (e -> e)         -- ^ element modifier 
                              -> a (Int, Int) e   -- ^ an array         
                              -> a (Int, Int) e   -- ^ modified array   
transform' t f m = foldr (\l m -> transform t f m l) m (gindices (no t) m)   

-- | Normalize a line of a matrix with Fractional elements
normalize :: (Fractional e, IArray a e) => Line                 -- ^ col row selector 
                                           -> Int               -- ^ line number
                                           -> a (Int, Int) e    -- ^ an array         
                                           -> a (Int, Int) e    -- ^ modified array 
normalize t r m = transform t (/factor) m r where
  factor = sum $ (lget t m r) -- the normalizing weighted factor

-- | Normalize all lines of a matrix
normalize' :: (Fractional e, IArray a e) => Line                 -- ^ col row selector 
                                            -> a (Int, Int) e    -- ^ an array         
                                            -> a (Int, Int) e    -- ^ modified array
normalize' t m  = foldr (\l m -> normalize t l m) m (gindices (no t) m)


