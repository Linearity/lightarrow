{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Simulation.Lightarrow.Task where

import              Control.Applicative
import              Control.Monad.Cont
import              Control.Monad.Reader
import              Control.Monad.State.Strict
import              Control.Monad.Writer.Strict
import              Data.Tuple
import qualified    Data.Bifunctor as BF
import              Data.Bitraversable
import qualified    Data.Lightarrow.IdentityList as IL
import              Data.MonadicStreamFunction.InternalCore
import qualified    FRP.BearRiver as A (identity)
import              FRP.BearRiver hiding (first, second, embed)
import              Optics

instance MonadIO m => MonadIO (Task a b m) where
    liftIO = lift . liftIO

instance MonadState s m => MonadState s (Task a b m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (Task a b m) where
    tell = lift . tell
    listen = mapTask changeMSF
        where changeMSF (MSF f) = MSF (\a -> do     ((ebc, k), w)   <- listen (f a)
                                                    return (right (, w) ebc, changeMSF k))
    pass = mapTask changeMSF
        where changeMSF (MSF f) = MSF (\a -> do     (ebcg, k)   <- f a
                                                    case ebcg of
                                                        Left b          -> return (Left b, changeMSF k)
                                                        Right (c, g)    -> do   c'  <- pass (return (c, g))
                                                                                return (Right c', changeMSF k))
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
voice m = callCC (\ cc -> ContT (\ k -> void (unVoice (liftA2 (,) (Voice m) (Voice (runContT (cc ()) k))))))

-- | Continue the lead voice with its next task
lead :: (MonadFix m, Monoid b) => Task a b m c -> ContT () (Task a b m) c
lead = lift

-- | Start a new voice whose output is always a monoidal identity; observe only
-- its monadic effects, not its output.
rest :: (MonadFix m, Monoid b1) => Task a b2 m c -> ContT () (Task a b1 m) ()
rest m = voice (embedTask id (const mempty) m)

-- | Transform a task that communicates on a bus via extra input and output
-- signals into a task that does so via monadic actions.
bus :: (Monad m, Monoid c) => Task (c, a) (c, b) m d -> Task a b (BusT c m) d
bus = mapTask (readerSF . writerSF . (>>> arr swizzle))
    where   swizzle ewbd = either (BF.second Left) (\d -> (mempty, Right d)) ewbd

-- | Transform a task that communicates on a bus via monadic actions into a
-- task that does so via extra input and output signals.
runBus :: (Monad m, Monoid c) => Task a b (BusT c m) d -> Task (c, a) (c, b) m d
runBus = mapTask ((>>> arr swizzle) . runWriterSF . runReaderSF)
    where   swizzle (w, ebd) = BF.first (w,) ebd

-- | Transform a task that communicates on a bus via monadic actions into a
-- task that does not communicate on a bus.
newBus :: (Monad m, Monoid c) => Task a b (BusT c m) d -> Task a b m d
newBus = mapTask (runWriterSF_ . runReaderSF_ mempty)

-- | Transform any task into one that connect to a bus but does not read or
-- write to it.
noBus :: (Monad m, Monoid c) => Task a b m d -> Task a b (BusT c m) d
noBus = mapTask (liftSF . liftSF)


-- | Start a new voice connected to a bus
busVoice :: (MonadFix m, Monoid c, Monoid b) =>
                Task a b (BusT c m) d
                    -> ContT () (Task a b (BusT c m)) ()
busVoice m
    = callCC (\cc ->
                ContT (\k ->
                        bus
                            (unBusVoice
                                (liftA2 (const (const ()))
                                        (BusVoice (runBus m))
                                        (BusVoice (runBus   (runContT (cc ())
                                                            k)))))))

-- | A variant of 'busMix' that mixes two tasks that communicate on a bus
-- via monadic actions.
busMixM :: (MonadFix m, Monoid c, Monoid b) =>
                Task a b (BusT c m) d
                    -> Task a b (BusT c m) e
                    -> Task a b (BusT c m) (d, e)
busMixM m1 m2 = bus (busMix (runBus m1) (runBus m2))

-- | A variant of 'mix' that mixes two tasks that communicate on a bus via
-- extra input and output signals.
busMix ::  (MonadFix m, Monoid b, Monoid c) =>
                Task (c, a) (c, b) m d
                    -> Task (c, a) (c, b) m e
                    -> Task (c, a) (c, b) m (d, e)
busMix m1 m2 = let BusVoice m = liftA2 (,) (BusVoice m1) (BusVoice m2) in m

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
firstOutputToBus :: (Monoid c, Monad m) =>
                        SF (BusT c m) (Either (c, a) b) (Either a b)
firstOutputToBus = arrM (bitraverse (\(c, b) -> tellBus c >> return b) return)

-- | A signal function that takes the second normal output signal of a task's
-- representative and writes it to the bus
secondOutputToBus :: (Monoid c, Monad m) =>
                        SF (BusT c m) (Either (a, c) b) (Either a b)
secondOutputToBus = arrM (bitraverse (\(b, c) -> tellBus c >> return b) return)

-- | Read the value on the bus
askBus :: (Monoid a, MonadReader a m) => ClockInfo m a
askBus = lift ask

-- | Read a portion of the value on the bus
asksBus :: (Monoid a, MonadReader a m) => (a -> b) -> ClockInfo m b
asksBus = lift . asks

-- | Write a value to the bus
tellBus :: (Monoid a, MonadWriter a m) => a -> ClockInfo m ()
tellBus = tell;

-- | A task that reads from and writes to a bus via monadic actions.
type BusTask c a b m = Task a b (BusT c m)

-- | A monad transformer that adds both a writer layer and then a reader layer.
-- Its purpose is to allow tasks to communicate on a bus via monadic actions.
type BusT c m = ReaderT c (WriterT c m)

-- | Tasks that run in parallel with each other, communicate with each
-- other via a bus, and process some changing state variable.
newtype StateBusVoice a b s c m d
        = StateBusVoice { unStateBusVoice :: Task a b (StateT s (BusT c m)) d }
    deriving (Functor, Monad)

instance (Monoid b, Monoid c, MonadFix m) =>
            Applicative (StateBusVoice a b s c m) where
    pure d = StateBusVoice (pure d)
    liftA2 f (StateBusVoice m1) (StateBusVoice m2)
            = StateBusVoice
                (mapTask (morphS commuteStateBus)
                    (bus (unBusVoice (liftA2 f (BusVoice m1') (BusVoice m2')))))
        where   m1'     = runBus (mapTask (morphS commuteBusState) m1)
                m2'     = runBus (mapTask (morphS commuteBusState) m2)

commuteBusState :: Monad m =>
                    ReaderT DTime (StateT s (ReaderT r (WriterT w m))) a
                        -> ReaderT DTime (ReaderT r (WriterT w (StateT s m))) a
commuteBusState (ReaderT f)
    = ReaderT (\dt ->
        ReaderT (\r ->
            WriterT (
                StateT (\s ->
                    fmap (\((a, s), w) -> ((a, w), s))
                        (runWriterT (runReaderT (runStateT (f dt) s) r))))))

commuteStateBus :: Monad m =>
                    ReaderT DTime (ReaderT r (WriterT w (StateT s m))) a
                        -> ReaderT DTime (StateT s (ReaderT r (WriterT w m))) a
commuteStateBus (ReaderT f)
    = ReaderT (\dt ->
        StateT (\s ->
            ReaderT (\r ->
                WriterT (
                    fmap (\((a, w), s) -> ((a, s), w))
                        (runStateT (runWriterT (runReaderT (f dt) r)) s)))))

-- stateTask :: Monad m => Task (s, a) (s, b) m c -> Task a b (StateT s m) c
stateTask :: Monad m => Task (s, a) (s, b) m (s, c) -> Task a b (StateT s m) c
stateTask = mapTask (\sf -> stateSF $ proc (s0, a) -> do
                                        esbc <- sf -< (s0, a)
                                        case esbc of
                                            Left (s1, b)    -> returnA -< (s1, Left b)
                                            Right (s1, c)   -> returnA -< (s1, Right c))
                                            -- Right c         -> returnA -< (s0, Right c))

-- runStateTask :: Monad m => Task a b (StateT s m) c -> Task (s, a) (s, b) m c
runStateTask :: Monad m => Task a b (StateT s m) c -> Task (s, a) (s, b) m (s, c)
runStateTask = mapTask (\sf -> proc (s0, a) -> do
                                (s1, ebc) <- runStateSF sf -< (s0, a)
                                returnA -< BF.bimap (s1,) (s1,) ebc)
                                -- returnA -< BF.first (s1,) ebc)

mapStateTask :: (Monad m, Monad n) =>
                    (forall a b c . Task a b m c -> Task a b n c)
                        -> Task a b (StateT s m) c
                        -> Task a b (StateT s n) c
mapStateTask f = stateTask . f . runStateTask

focusStateT :: Monad m => Lens' s2 s1 -> StateT s1 m a -> StateT s2 m a
focusStateT l x = StateT (\s2 -> do     (a, s1)     <- runStateT x (s2 ^. l)
                                        return (a, s2 & l .~ s1))

vainMixList :: (Monad m, Monoid b) => [Task a b m c] -> Task a b m (c, [Task a b m c])
vainMixList = vainMix
    --     = Task (parB (map (vain . taskRep) ts) >>> arr raceAll)
    -- where   raceAll                         = finish . foldr race (Left mempty, [])
    --         finish (Left bs, _)             = Left bs
    --         finish (Right c, ks)            = Right (c, ks)
    --         race (Left b, k) (Left bs, ks)  = (Left (b <> bs), Task k : ks)
    --         race (Right c, k) (_, ks)       = (Right c, Task k : ks)
    --         race (_, k) (Right c, ks)       = (Right c, Task k : ks)

vainMix ts
        = Task (parB (fmap (vain . taskRep) ts) >>> arr raceAll)
    where   raceAll                         = finish . (\ebcks -> (foldr race (Left mempty) (fmap fst ebcks),
                                                                    fmap (Task . snd) ebcks))
            finish (Left bs, _)             = Left bs
            finish (Right c, ks)            = Right (c, ks)
            race (Left b) (Left bs)         = Left (b <> bs)
            race (Right c) _                = Right c
            race _ (Right c)                = Right c

vainMixIL :: (Monad m, Monoid b) =>
                IL.IL (Task a b m c)
                    -> Task
                        a
                        (b, IL.IL (Task a b m c))
                        m
                        ((Int, c), IL.IL (Task a b m c))
vainMixIL ts
        = Task (parB (fmap (vain . taskRep) ts) >>> arr raceAll)
    where   raceAll                     = finish . (\ebcks -> (IL.keyFoldr race (Left mempty) (fmap fst ebcks),
                                                                fmap (Task . snd) ebcks))
            finish (Left bs, ks)        = Left (bs, ks)
            finish (Right nc, ks)       = Right (nc, ks)
            race (_, Left b) (Left bs)  = Left (b <> bs)
            race (n, Right c) _         = Right (n, c)
            race _ x                    = x