{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Simulation.Lightarrow.Mode where

import              Control.Applicative
import              Control.Monad.Cont hiding (join)
import              Control.Monad.Reader hiding (join)
import              Control.Monad.State hiding (join, StateT)
import              Control.Monad.Writer.Strict
import              Data.Tuple
import qualified    Data.Bifunctor as BF
--import qualified    FRP.BearRiver as A (first, second)
import qualified    FRP.BearRiver as A (identity)
import              FRP.BearRiver hiding (first, second, embed)
{-

A mode is a section of a signal function. It begins at local time zero and terminates upon some later event.

-}
newtype Mode a b m c = Mode { modeRep :: SF m a (Either b c) }
{-

A mode can be combined with a final continuation to produce an ordinary signal function:

-}
runMode :: Monad m => Mode a b m c -> (c -> SF m a b) -> SF m a b
runMode (Mode sf) = switch (sf >>> arr eitherToEvent)
    where   eitherToEvent x     = case x of
                                    Left b      -> (b, NoEvent)
                                    Right c     -> (undefined, Event c)

branch :: Monad m => SF m a (Either b c)
                        -> SF m (a, b) d
                        -> SF m (a, c) d
                        -> SF m a d
branch sf sfL sfR = proc a -> do
                        ebc <- sf -< a
                        case ebc of
                            Left b  -> sfL -< (a, b)
                            Right c -> sfR -< (a, c)
{-

Or it can loop forever:

-}
loopMode :: Monad m => Mode a b m c -> SF m a b
loopMode a = runMode (forever a) (const (loopMode a))
{-

In this way we can relate those expressions that denote signal functions to those that denote modes.

A mode is a monad, and as such it is also an applicative functor. The context implicit in this is that of the current signal function: two modes combined with |(>>=)| or |(<*>)| mean that the current signal function switches from the first mode to the second mode over time.

-}
instance Monad m => Functor (Mode a b m) where
    fmap f (Mode sf) = Mode (sf >>> arr (fmap f))

instance Monad m => Applicative (Mode a b m) where
    pure c  = Mode (constant (Right c))
    Mode sf1 <*> x  = Mode (sf1 `link` (\f -> modeRep (fmap f x)))


instance Monad m => Monad (Mode a b m) where
    return c        =  Mode (constant (Right c))
    (>>=)           =  bind

bind (Mode sf1) f = Mode (sf1 `link` (modeRep . f))

instance MonadTrans (Mode a b) where
    lift = blink . const . lift

instance MonadState s m => MonadState s (Mode a b m) where
    get     = lift get
    put     = lift . put
    state   = lift . state
{-

We can define some common patterns as Modes. One is output that occurs over an infinitesimal interval, such as an impulse:

-}
moment :: Monad m => (a -> ClockInfo m b) -> Mode a b m ()
moment f = Mode (arrM f &&& (now () >>> iPre NoEvent) >>> arr eitherEvent)
{-

Here we have transformed a value of type |(b, Event c)| to type |Either b c| using |eitherEvent|.

-}
eitherEvent :: (b, Event c) -> Either b c
eitherEvent (b, NoEvent) = Left b
eitherEvent (_, Event c) = Right c
{-

|moment| takes a function as an argument, but often we want just a momentary value that does not depend on the input. We can apply |momentC| to such a value.

-}
momentC :: Monad m => b -> Mode a b m ()
momentC b = moment (const (return b))


blink :: Monad m => (a -> ClockInfo m c) -> Mode a b m c
blink g = Mode (arrM (g >=> return . Right))


sample :: Monad m => Mode a b m a
sample = blink return

before :: Monad m => SF m a (Event c) -> SF m a b -> Mode a b m c
before interrupt sf = Mode (sf &&& interrupt >>> arr eitherEvent)

over :: Monad m => DTime -> SF m a b -> Mode a b m ()
over interval sf = Mode ( proc a -> do
                            b   <- sf       -< a
                            t   <- time     -< ()
                            returnA -< if t < interval then Left b else Right ())

wait :: (Monad m, Monoid b) => DTime -> Mode a b m ()
wait interval = over interval (constant mempty)
{-

Some activities should never terminate:

-}
always :: Monad m => SF m a b -> Mode a b m c
always sf = Mode (sf >>> arr Left)
{-

There are other termination conditions beyond the passing of some interval. In general we can run a signal function until the rising edge of some Boolean signal.

-}
rising p = arrM p >>> edge
falling p = arrM (fmap not . p) >>> edge
indeed p = arrM p >>> iEdge False
indeedNot p = arrM (fmap not . p) >>> iEdge False

onlyUntil :: Monad m => SF m (a, b) (Event c) -> Mode a b m c -> Mode a b m c
onlyUntil condition = mapMode clip
    where clip sf = branch sf (proc (a, b) -> do
                                    stop <- condition -< (a, b)
                                    returnA -< event (Left b) Right stop)
                              (arr (Right . snd))

lastOut :: MonadFix m => b -> Mode a b m c -> Mode a b m (c, b)
lastOut b0 = mapMode (record b0)
    where record b0 sf = proc a -> do
                            rec     bR      <-  iPre b0     -< b
                                    ebc     <-  sf          -< a
                                    let b = case ebc of
                                                Left b      -> b
                                                Right _c    -> bR
                            returnA -< fmap (,b) ebc

vainMode :: Monad m => Mode a b m c -> Mode a b m (c, Mode a b m c)
vainMode (Mode sf0) = Mode sf
    where sf = proc a -> do
                (ebc, sf1) <- vain sf0 -< a
                returnA -< (fmap (, Mode sf1) ebc)
{-

We can also compute transformations of modes.

-}
mapMode :: (Monad m1, Monad m2) =>
                (SF m1 a (Either b c) -> SF m2 d (Either e f))
                    -> Mode a b m1 c
                    -> Mode d e m2 f
mapMode f (Mode sf) = Mode (f sf)

firstM :: Applicative m => (a -> m c) -> Either a b -> m (Either c b)
firstM f (Left a)   = fmap Left (f a)
firstM _f (Right b) = pure (Right b)

secondM :: Applicative m => (b -> m c) -> Either a b -> m (Either a c)
secondM _f (Left a) = pure (Left a)
secondM f (Right b) = fmap Right (f b)

firstA :: ArrowChoice a => a b c -> a (Either b d) (Either c d)
firstA sf = proc ebc -> case ebc of
                            Left b      -> arr Left <<< sf -< b
                            Right c     -> returnA -< Right c

secondA :: ArrowChoice a => a b c -> a (Either d b) (Either d c)
secondA sf = proc ebc -> case ebc of
                            Left b      -> returnA -< Left b
                            Right c     -> arr Right <<< sf -< c

data Threether a b c = Links a | Mitte b | Rechts c

newtype Voice a b m c = Voice { unVoice :: Mode a b m c }
    deriving (Functor, Monad)

instance (Monad m, Monoid b) => Applicative (Voice a b m) where
    pure c = Voice (pure c)
    liftA2 f (Voice (Mode sf1)) (Voice (Mode sf2))
            = Voice     (do    t  <- Mode  (proc a -> do
                                               (ebc1, r1)  <- vain sf1     -< a
                                               (ebc2, r2)  <- vain sf2     -< a
                                               returnA -< case (ebc1, ebc2) of
                                                               (Left b1, Left b2)  -> Left (b1 <> b2)
                                                               (Right c, Left _)   -> Right (Links (c, r2))
                                                               (Left _, Right d)   -> Right (Mitte (d, r1))
                                                               (Right c, Right d)  -> Right (Rechts (c, d)))
                               case t of
                                   Links (c, r)     -> do   d  <- Mode r
                                                            return (f c d)
                                   Mitte (d, r)     -> do   c  <- Mode r
                                                            return (f c d)
                                   Rechts (c, d)    -> return (f c d))

mix :: (MonadFix m, Monoid b) => Mode a b m c -> Mode a b m d -> Mode a b m (c,d)
mix m1 m2 = unVoice (liftA2 (,) (Voice m1) (Voice m2))
{-

Simultaneous modes with a secondary input/output channel may communicate with each other automatically on this channel using a bus.

-}
type BusMode c a b m = Mode a b (ReaderT c (WriterT c m))

newtype BusVoice a b c m d = BusVoice { unBus :: Mode (a, c) (b, c) m d }
    deriving (Functor, Monad)

busMix ::  (MonadFix m, Monoid b, Monoid c)
            =>  Mode (a, c) (b, c) m d
                -> Mode (a, c) (b, c) m e
                -> Mode (a, c) (b, c) m (d, e)
busMix m1 m2 = let BusVoice m = liftA2 (,) (BusVoice m1) (BusVoice m2) in m

instance (MonadFix m, Monoid b, Monoid c) => Applicative (BusVoice a b c m) where
    pure c = BusVoice (pure c)
    liftA2 f (BusVoice (Mode sf1)) (BusVoice (Mode sf2))
        = BusVoice (do  t   <- Mode sf
                        case t of
                            Links (c, r)    -> do   d   <- Mode r
                                                    return (f c d)
                            Mitte (d, r)    -> do   c   <- Mode r
                                                    return (f c d)
                            Rechts (c, d)   -> return (f c d))
            where   sf  = proc (a, c) -> do
                            rec c1'                   <- iPre mempty  -< c1
                                c2'                   <- iPre mempty  -< c2
                                (ebcd1, k1)  <- vain sf1     -< (a, c <> c2')
                                (ebcd2, k2)  <- vain sf2     -< (a, c <> c1')
                                let c1 = either snd (const mempty) ebcd1
                                    c2 = either snd (const mempty) ebcd2
                            returnA -< (case (ebcd1, ebcd2) of
                                            (Left (b1, c1), Left (b2, c2))  -> Left (b1 <> b2, c1 <> c2)
                                            (Right c, Left _)               -> Right (Links (c, k2))
                                            (Left _, Right d)               -> Right (Mitte (d, k1))
                                            (Right c, Right d)              -> Right (Rechts (c, d)))

busMixM m1 m2 = bus (mapMode busSwap
                        (busMix (mapMode busSwap (runBus m1))
                                (mapMode busSwap (runBus m2))))

type BusT c m = ReaderT c (WriterT c m)

newBus :: (Monad m, Monoid c) =>
            Mode a b (ReaderT c (WriterT c m)) d
                -> Mode a b m d
newBus = mapMode (runWriterSF_ . runReaderSF_ mempty)

noBus :: (Monad m, Monoid c) =>
            Mode a b m d
                -> Mode a b (ReaderT c (WriterT c m)) d
noBus = mapMode (liftReaderSF . liftWriterSF)

bus :: (Monad m, Monoid c) =>
        Mode (c, a) (c, b) m d
            -> Mode a b (ReaderT c (WriterT c m)) d
bus = mapMode (readerSF . writerSF . (>>> arr swizzle))
    where   swizzle ewbd = either (\(w,b) -> (w, Left b)) (\d -> (mempty, Right d)) ewbd

runBus :: (Monad m, Monoid c) =>
            Mode a b (ReaderT c (WriterT c m)) d
                -> Mode (c, a) (c, b) m d
runBus = mapMode ((>>> arr swizzle) . runWriterSF . runReaderSF)
    where   swizzle (w, ebd) = BF.first (w,) ebd

busSwap :: Monad m =>
            SF m (a1, a2) (Either (b1, b2) c)
                -> SF m (a2, a1) (Either (b2, b1) c)
busSwap sf = arr swap >>> sf >>> arr (BF.first swap)



embedMode fI fO = mapMode f
    where   f sf   = arr fI >>> sf >>> arr (BF.first fO)
{-

Simultaneous activities can be layered together as concurrent "threads."

-}
chorus t = runContT t return
voice m = callCC (\ cc -> ContT (\ k -> void (mix m (runContT (cc ()) k))))
rest m = voice (embedMode id (const mempty) m)
busVoice m
    = callCC (\cc -> ContT (\k -> void (busMixM m (runContT (cc ()) k))))

firstInputFromBus :: (Monoid c, Monad m) => SF (ReaderT c (WriterT c m)) a (c, a)
firstInputFromBus = constM askBus &&& A.identity

secondInputFromBus :: (Monoid c, Monad m) => SF (ReaderT c (WriterT c m)) a (a, c)
secondInputFromBus = A.identity &&& constM askBus

firstOutputToBus :: (Monoid c, Monad m) => SF (ReaderT c (WriterT c m)) (Either (c, a) b) (Either a b)
firstOutputToBus = arrM (firstM (\(c, b) -> tellBus c >> return b))

secondOutputToBus :: (Monoid c, Monad m) => SF (ReaderT c (WriterT c m)) (Either (a, c) b) (Either a b)
secondOutputToBus = arrM (firstM (\(b, c) -> tellBus c >> return b))

askBus :: (Monoid a, Monad m) => ClockInfo (ReaderT a (WriterT a m)) a
askBus = lift ask

asksBus :: (Monoid a, Monad m) => (a -> b) -> ClockInfo (ReaderT a (WriterT a m)) b
asksBus = lift . asks

tellBus :: (Monoid a, Monad m) => a -> ClockInfo (ReaderT a (WriterT a m)) ()
tellBus = tell;