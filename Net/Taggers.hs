-- | Some valid taggers for Net type
module Net.Taggers where

import Net.Net


data Tagger = DumbTag' DumbTag deriving (Read)

unTag :: Tagger -> (forall a.(Tag a) => a -> ret) -> ret
unTag (DumbTag' x) fn = fn x


-- | This tagger uses its wrapped Int "n" as a cut limit.
-- First n values will receive 1 and be promoted , the rest 0 and will be fired
newtype DumbTag = DumbTag {unDumbTag :: Int}   deriving (Read)

instance Tag DumbTag where
  tag n qs = take (unDumbTag n) (repeat 1) ++ (repeat 0)

