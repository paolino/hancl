module Net.Worker where

import Control.Monad.State
import Control.Monad.Reader
import System.Random (StdGen)
import Prelude hiding (log)

data WState s w = WState {wstate :: s, wlog :: w -> w, wrandom :: StdGen}

newtype Worker e s w a = Worker (ReaderT e (State (WState s w)) a) deriving (Functor,Monad,MonadReader e)

instance MonadState s (Worker e s w)  where
  get = Worker (lift $ liftM wstate get)
  put x = Worker (lift $ modify (\ws -> ws{wstate = x}))     


tell w = Worker (lift $ modify (\ws@WState{wlog = l} -> ws{wlog=(w:).l} ))

runrand action = Worker $ lift $ do ws@WState{wrandom = g} <- get
                                    let (a,g') = runState action g
                                    put ws{wrandom = g'}
                                    return a

data Result a s w = Result {result :: a, state :: s, log :: [w], generator :: StdGen} deriving Show
runWorker (Worker action) environment initialwstate gen = 
  let (a,s) = runState (runReaderT action environment) $ WState initialwstate id gen in
      Result a (wstate s) (wlog s $ []) (wrandom s)


