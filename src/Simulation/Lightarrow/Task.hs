{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Simulation.Lightarrow.Task where

import              Control.Applicative
import              Control.Monad.Cont hiding (join)
import              Control.Monad.Reader hiding (join)
import              Control.Monad.State hiding (join, StateT)
import              Control.Monad.Writer.Strict
import              Data.Tuple
import qualified    Data.Bifunctor as BF
import              Data.Bitraversable
import qualified    FRP.BearRiver as A (identity)
import              FRP.BearRiver hiding (first, second, embed)

instance MonadState s m => MonadState s (Task a b m) where
    get = lift get
    put = lift . put

rising p = arrM p >>> edge
falling p = arrM (fmap not . p) >>> edge
indeed p = arrM p >>> iEdge False
indeedNot p = arrM (fmap not . p) >>> iEdge False

vainTask :: Monad m => Task a b m c -> Task a b m (c, Task a b m c)
vainTask (Task sf0) = Task sf
    where sf = proc a -> do
                (ebc, sf1) <- vain sf0 -< a
                returnA -< (fmap (, Task sf1) ebc)

embedTask fI fO = mapTask f
    where   f sf   = arr fI >>> sf >>> arr (BF.first fO)
{-

Simultaneous activities can be layered together as concurrent "threads."

-}
chorus t = runContT t return
voice m = callCC (\ cc -> ContT (\ k -> void (mix m (runContT (cc ()) k))))
rest m = voice (embedTask id (const mempty) m)
busVoice m
    = callCC (\cc -> ContT (\k -> void (busMixM m (runContT (cc ()) k))))

firstInputFromBus :: (Monoid c, Monad m) => SF (ReaderT c (WriterT c m)) a (c, a)
firstInputFromBus = constM askBus &&& A.identity

secondInputFromBus :: (Monoid c, Monad m) => SF (ReaderT c (WriterT c m)) a (a, c)
secondInputFromBus = A.identity &&& constM askBus

firstOutputToBus :: (Monoid c, Monad m) => SF (ReaderT c (WriterT c m)) (Either (c, a) b) (Either a b)
firstOutputToBus = arrM (bitraverse (\(c, b) -> tellBus c >> return b) return)

secondOutputToBus :: (Monoid c, Monad m) => SF (ReaderT c (WriterT c m)) (Either (a, c) b) (Either a b)
secondOutputToBus = arrM (bitraverse (\(b, c) -> tellBus c >> return b) return)

askBus :: (Monoid a, Monad m) => ClockInfo (ReaderT a (WriterT a m)) a
askBus = lift ask

asksBus :: (Monoid a, Monad m) => (a -> b) -> ClockInfo (ReaderT a (WriterT a m)) b
asksBus = lift . asks

tellBus :: (Monoid a, Monad m) => a -> ClockInfo (ReaderT a (WriterT a m)) ()
tellBus = tell;