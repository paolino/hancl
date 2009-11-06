module Net.Schedulers where
import Datas 
import System.Random
import Net.Net
import qualified Data.Set as Set
import Net.Node 


data Scheduler = NoScheduling' NoScheduling | SimpleScheduling' SimpleScheduling deriving Read

unScheduler :: Scheduler -> (forall a.(Schedule a,Ord a) => a -> ret) -> ret

unScheduler (NoScheduling' x) fn = fn x
unScheduler (SimpleScheduling' x) fn = fn x


newtype NoScheduling = NoScheduling Float deriving (Random,Num,Show,Eq,Ord,Read)

instance  Schedule NoScheduling where
  zero = NoScheduling 0
  schedule _ _ = 1
  mediate _ = id


newtype SimpleScheduling = SimpleScheduling {unSimpleScheduling::Float} deriving (Random,Num,Show,Eq,Ord,Read)  

instance Schedule SimpleScheduling where
  zero = SimpleScheduling 0
 
  schedule n n' = SimpleScheduling $ 1 - exp (-change - 0.01) where
    change = fromIntegral. Set.size $ Set.difference (set n) (set n')
    set n = Set.fromList . map fst $ links  n

  mediate k ss = 
    map (SimpleScheduling.(+ k* unSimpleScheduling (sum ss)/fromIntegral (length ss)).(*(1-k)).unSimpleScheduling) ss
 



