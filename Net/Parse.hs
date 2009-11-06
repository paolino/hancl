module Net.Parse where

import Net.Net
import Control.Arrow
import Net.Schedulers
import Net.Taggers
import Net.Refiners
import Net.Collectors
import Datas
import Text.Regex
import System.Random

type Launcher ret = 
  (forall h c t r.(Schedule h,Ord h,Collect c,Tag t,Refine r) => Int -> Float -> h -> c -> t -> r -> ret) -> ret

parsePolitics :: String -> String -> String -> String -> String -> String -> Launcher ret
parsePolitics refining tagging collecting scheduling memory dissipation  =  \f ->   
  unRefine (read refining) (unTag (read tagging) (unCollect (read collecting) (unScheduler (read scheduling) 
    (f (read memory) (read dissipation))))) 
  
parseConfig :: String -> Launcher ret
parseConfig string = let [memory,dissipation,refining,tagging,collecting,scheduling,_] = lines string in 
     parsePolitics refining tagging collecting scheduling memory dissipation

parseDatas :: String -> String -> (forall a.Metrics a => [a] -> ret) -> ret
parseDatas tstring string = unData (map (read.subRegex (mkRegex "%") tstring) $ lines string) 

parseAndRun :: String 
               -> String 
               -> StdGen 
               -> (forall s h r q e.(Refine e, Tag q, Collect r, Schedule h, Datas.Metrics s) => Net s h r q e [w] a) 
               -> (String,[w])
parseAndRun config datas generator action = let 
  [memory,_,_,_,_,_,datastype] = lines config 
  missile datas = (show *** id) $ (parseConfig config) (run generator action (create 3 datas))
    in parseDatas datastype datas missile 
         
    
