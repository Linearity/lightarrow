module Simulation.Lightarrow.Event where

import Control.Monad.Fix
import Data.VectorSpace
import FRP.BearRiver
import Control.Applicative
{-

We define some common event sources.

-}
afterInput :: Monad m => a -> SF m Time (Event a)
afterInput x =  proc threshold -> do
                  t     <-  time  -< ()
                  done  <-  edge  -< t >= threshold
                  returnA -< done `tag` x
{-

|restart| maps any signal function to a signal function whose local time
restarts at 0 upon a given event.

-}
restart :: Monad m => SF m a b -> SF m (a, Event c) b
restart sf = first sf `switch` const (second notYet >>> restart sf)
{-

|periodically| combines |restart| and |afterInput| to create a repeating event
source whose period is externally controlled.

-}
periodically :: MonadFix m => a -> SF m Time (Event a)
periodically x = proc p -> do
                    rec occur'  <-  iPre NoEvent            -< occur
                        occur   <-  restart (afterInput x)  -< (p, occur')
                    returnA -< occur

instance Semigroup a => Semigroup (Event a) where
    Event a1    <> Event a2     = Event (a1 <> a2)
    NoEvent     <> e2           = e2
    e1          <> _            = e1

instance Semigroup a => Monoid (Event a) where
    mempty = NoEvent

instance Alternative Event where
    empty   = NoEvent
    NoEvent     <|> Event a2    = Event a2
    e1          <|> _           = e1