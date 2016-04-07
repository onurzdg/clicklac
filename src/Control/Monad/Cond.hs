module Control.Monad.Cond where

import Control.Monad (Monad, liftM)
import Data.Bool (bool)   

ifM :: Monad m => m b -> m b -> m Bool -> m b
ifM f s condM = bool f s =<< condM            

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM m = bool (return ()) m =<< condM

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM = whenM (liftM not condM) 
        
maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM def fn ma = maybe def fn =<< ma
