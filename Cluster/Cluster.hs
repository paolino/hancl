module Cluster.Cluster where 

import Control.Arrow
import Cluster.Core
import Lib.Matrix
import Datas

-- super stupid clusterer
scluster :: Metrics a => Points a -> Points a -> Clusters a
scluster ps cs = app . (snd &&& fst) $ boot ps cs

-- normalizing clusterer
ncluster :: Metrics a => Points a -> Points a -> Clusters a
ncluster ps cs = app .(snd &&& normalize' Row . fst) $ boot ps cs 

-- wcluster :: (Metrics a) => (DM Float -> DM Float) -> Points a -> Points a -> Clusters a
-- reweight is missing in module Matrix  




