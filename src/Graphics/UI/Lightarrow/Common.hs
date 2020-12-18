module Graphics.UI.Lightarrow.Common
    (   oneShot,
        pushdown,
        toggle,
        drag,
        xfCursor,
        mouseClick,
        mouseOver   )   where

import Control.Monad.State
import Data.MonadicStreamFunction hiding (embed)
import FRP.BearRiver hiding (embed)
import Optics
import Simulation.Lightarrow.Task

{-|

An activity that "fires" an event given some event it detects in the input.
Upon this event occurrence it goes idle until it detects another, possibly
different event and resets itself.  Then it can fire again.

-}
oneShot :: Monad m =>
                SF m a (Event b)                        -- ^ "firing" event detector
                    -> SF m a (Event c)                 -- ^ "reset" event detector
                    -> (a -> ClockInfo m (Event d))     -- ^ output event based on input
                    -> Task a (Event d) m b             -- ^ firing activity
oneShot shoot reset f = toggle shoot fire reset idle
    where   fire    = moment f >> always (constant NoEvent)
            idle    = always (constant NoEvent)

-- | An activity that is in one of two modes depending on some state of the input
pushdown :: Monad m =>
                (a -> ClockInfo m Bool)     -- ^ control state based on input
                    -> Task a b1 m ()       -- ^ "on" mode
                    -> Task a b1 m ()       -- ^ "off" mode
                    -> Task a b1 m b2       -- ^ toggling activity
pushdown pushed on = toggle (indeed pushed) on (indeedNot pushed)

{-|

An activity that detects two events in the input and in each case switches
from one mode to the other or vice versa

-}
toggle :: Monad m =>
            SF m a (Event c)            -- ^ on-to-off detector
                -> Task a b m d         -- ^ "on" mode
                -> SF m a (Event d)     -- ^ off-to-on detector
                -> Task a b m c         -- ^ "off" mode
                -> Task a b m e         -- ^ toggling activity
toggle up on down off = forever (do  onlyUntil (arr fst >>> up) off
                                     onlyUntil (arr fst >>> down) on)

{-|

An activity, with an element's state as context, that moves the element
in the same way the the cursor moves

-}
drag :: MonadState s m =>
            Lens' a (Double, Double)                    -- ^ cursor position
                -> Lens' s (Double, Double, Double)     -- ^ element position
                -> Task a () m b                        -- ^ dragging activity
drag _cursor _x
        = do    a0  <- sample
                d   <- xfCursor _x (a0 ^. _cursor)
                always (arrM (move d))
    where move (xD, yD) a = do  let (xC, yC) = a ^. _cursor
                                modify (_x %~ (\x -> let    u = xC - xD
                                                            v = yC - yD 
                                                        in (u, v, x^._3)))

{-|

An action on an element's state that returns the cursor's position as
an offset to the element's position

-}
xfCursor _x (xC, yC) = do   (x, y, _z)  <- use _x
                            return (xC - x, yC - y)

{-|

A signal function, with an element's state as context, that detects a mouse
click on the element

-}
mouseClick :: ( MonadState s m,
                Fractional b,
                Fractional c,
                Ord b, Show b,
                Ord c, Show c  ) =>
                    Lens' a Bool                -- ^ button state
                        -> Lens' a (b, c)       -- ^ cursor position within input
                        -> Lens' s (b, c, d)    -- ^ element position within state
                        -> Lens' s (b, c)       -- ^ element dimensions within state
                        -> SF m a (Event ())    -- ^ detector
mouseClick _button _cursor _x _d
    = proc a -> do
        click   <- edge                             -< a ^. _button
        over    <- arrM (mouseOver _cursor _x _d)   -< a
        returnA -< gate click over

{-|

An action on an element's state that returns whether the mouse cursor overlaps
the element

-}
mouseOver :: (MonadState s m, Fractional b, Fractional c, Ord b, Ord c, Show b, Show c) =>
                Lens' a (b, c)                  -- ^ cursor position
                    -> Lens' s (b, c, d)        -- ^ element position
                    -> Lens' s (b, c)           -- ^ element dimensions
                    -> a                        -- ^ input
                    -> m Bool                   -- ^ state action
mouseOver _cursor _x _d a = do  (x, y, _z)  <- gets (view _x)
                                (w, h)      <- gets (view _d)
                                let   x'  = x - w/2
                                      y'  = y - h/2
                                return (inBox w h (x', y') (a ^. _cursor))

inBox width height (bx, by) (x, y) = inX && inY
  where
    inX = bx < x && x < bx + width
    inY = by < y && y < by + height