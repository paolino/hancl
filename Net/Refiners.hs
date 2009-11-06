-- | A collection of Refiner instances

module Net.Refiners where

import Net.Net
import Control.Monad.Reader
import Net.Node
import Lib.Shuffle
import Net.Worker


data Refiner = First' First  deriving (Read)

unRefine :: Refiner -> (forall  a. (Refine a) => a -> ret ) -> ret
unRefine (First' x) fn = fn x

newtype First = First {unFirst::()}  deriving Read

instance Refine First  where
  refine i = do gs <- liftM (filter (/= i)) $ collect i   -- una collezione di vicini potenziali 
                vgs <- values gs                          -- i loro valori
                is <- liftM (links.inserts (zip gs vgs)) $ node i -- mescolati ai link del nodo ed estratti
                tf <- asks $ tag.etag         -- ricostruiamo il tagger e tagghiamo
                runrand $ judge is (tf $ map snd is)                  -- i links scelti da toccare dopo il tagging
