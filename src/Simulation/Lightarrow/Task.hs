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

-- | The rising edge of some condition
rising :: Monad m => (a -> ClockInfo m Bool) -> SF m a (Event ())
rising p = arrM p >>> edge

-- | The falling edge of some condition
falling :: Monad m => (a -> ClockInfo m Bool) -> SF m a (Event ())
falling p = arrM (fmap not . p) >>> edge

-- | The earliest moment of some condition
indeed :: Monad m => (a -> ClockInfo m Bool) -> SF m a (Event ())
indeed p = arrM p >>> iEdge False

-- | The earliest moment of the absence of some condition
indeedNot :: Monad m => (a -> ClockInfo m Bool) -> SF m a (Event ())
indeedNot p = arrM (fmap not . p) >>> iEdge False

-- | A task that represents its own possible continuation by returning
-- a subsequent task when it first terminates
vainTask :: Monad m => Task a b m c -> Task a b m (c, Task a b m c)
vainTask (Task sf0) = Task sf
    where sf = proc a -> do
                (ebc, sf1) <- vain sf0 -< a
                returnA -< (fmap (, Task sf1) ebc)

-- | Remap a task's input and output signals
embedTask :: Monad m => (c -> a) -> (b -> d) -> Task a b m f -> Task c d m f
embedTask fI fO = mapTask f
    where   f sf   = arr fI >>> sf >>> arr (BF.first fO)

-- | Join several voices together into one task
chorus :: Monad m => ContT r (Task a b m) r -> Task a b m r
chorus t = runContT t return

-- | Start a new voice concurrent with the lead voice
voice :: (MonadFix m, Monoid b) => Task a b m c -> ContT () (Task a b m) ()
voice m = callCC (\ cc -> ContT (\ k -> void (mix m (runContT (cc ()) k))))

-- | Continue the lead voice with its next task
lead :: (MonadFix m, Monoid b) => Task a b m c -> ContT () (Task a b m) c
lead = lift

-- | Start a new voice whose output is always a monoidal identity; observe only
-- its monadic effects, not its output.
rest :: (MonadFix m, Monoid b1) => Task a b2 m c -> ContT () (Task a b1 m) ()
rest m = voice (embedTask id (const mempty) m)

-- | Start a new voice connected to a bus
busVoice :: (MonadFix m, Monoid c, Monoid b) =>
                Task a b (BusT c m) d
                    -> ContT () (Task a b (BusT c m)) ()
busVoice m
    = callCC (\cc -> ContT (\k -> void (busMixM m (runContT (cc ()) k))))

-- | A signal function that retrieves the value on the bus and produces it as
-- its first /output/ signal
firstInputFromBus :: (Monoid c, Monad m) => SF (BusT c m) a (c, a)
firstInputFromBus = constM askBus &&& A.identity

-- | A signal function that retrieves the value on the bus and produces it as
-- its second /output/ signal
secondInputFromBus :: (Monoid c, Monad m) => SF (BusT c m) a (a, c)
secondInputFromBus = A.identity &&& constM askBus

-- | A signal function that takes the first normal output signal of a task's
-- representative and writes it to the bus
firstOutputToBus :: (Monoid c, Monad m) => SF (BusT c m) (Either (c, a) b) (Either a b)
firstOutputToBus = arrM (bitraverse (\(c, b) -> tellBus c >> return b) return)

-- | A signal function that takes the second normal output signal of a task's
-- representative and writes it to the bus
secondOutputToBus :: (Monoid c, Monad m) => SF (BusT c m) (Either (a, c) b) (Either a b)
secondOutputToBus = arrM (bitraverse (\(b, c) -> tellBus c >> return b) return)

-- | Read the value on the bus
askBus :: (Monoid a, Monad m) => ClockInfo (BusT a m) a
askBus = lift ask

-- | Read a portion of the value on the bus
asksBus :: (Monoid a, Monad m) => (a -> b) -> ClockInfo (BusT a m) b
asksBus = lift . asks

-- | Write a value to the bus
tellBus :: (Monoid a, Monad m) => a -> ClockInfo (BusT a m) ()
tellBus = tell;