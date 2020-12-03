module Graphics.UI.Lightarrow.Common where

import Control.Monad.State
import Data.VectorSpace
import Data.MonadicStreamFunction hiding (embed)
import FRP.BearRiver hiding (embed)
import Optics
import Simulation.Lightarrow.Mode

oneShot :: Monad m =>
                 SF m a1 (Event a2)
                 -> SF m a1 (Event a3)
                 -> (a1 -> ClockInfo m (Event a4))
                 -> Mode a1 (Event a4) m b
oneShot shoot reset f = toggle shoot fire reset idle
    where   fire    = moment f >> always (constant NoEvent)
            idle    = always (constant NoEvent)

pushdown :: Monad m =>
                  (a -> ClockInfo m Bool)
                  -> Mode a b1 m () -> Mode a b1 m () -> Mode a b1 m b2
pushdown pushed on = toggle (indeed pushed) on (indeedNot pushed)

toggle :: Monad m =>
                SF m a1 (Event a2)
                -> Mode a1 b1 m a3
                -> SF m a1 (Event a3)
                -> Mode a1 b1 m a2
                -> Mode a1 b1 m b2
toggle up on down off = forever (do  onlyUntil (arr fst >>> up) off
                                     onlyUntil (arr fst >>> down) on)

drag (_cursor :: Lens' a (Double, Double)) (_x :: Lens' b (Double, Double, Double))
        = do    a0  <- sample
                d   <- xfCursor _x (a0 ^. _cursor)
                always (arrM (move d))
    where move d a = modify (_x %~ (\x -> let (u, v) = (a ^. _cursor) ^+^ d
                                            in (u, v, x^._3)))

xfCursor _x c = do  (x, y, _z)  <- gets (view _x)
                    return ((x, y) ^-^ c)

mouseClick _button _cursor _x _d
    = proc a -> do
        click   <- edge                             -< a ^. _button
        over    <- arrM (mouseOver _cursor _x _d)   -< a
        returnA -< gate click over

mouseOver _cursor _x _d a = do  (x, y, _z)      <- gets (view _x)
                                (w, h)          <- gets (view _d)
                                let   x'  = x - w/2
                                      y'  = y - h/2
                                return (inBox w h (x', y') (a ^. _cursor))

inBox width height (bx, by) (x, y) = inX && inY
  where
    inX = bx < x && x < bx + width
    inY = by < y && y < by + height
