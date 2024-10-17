module Valere.Utils where

import Control.Monad

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM acc = do 
  cond <- condM 
  unless cond acc

ifM :: Monad m => m Bool -> m a -> m a -> m a 
ifM condM m1 m2 = do
  cond <- condM
  if cond then m1 else m2 