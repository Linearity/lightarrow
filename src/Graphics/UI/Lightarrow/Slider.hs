module Graphics.UI.Lightarrow.Slider (slider) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.MonadicStreamFunction hiding (embed)
import Graphics.UI.Lightarrow.Common
import Optics
import Simulation.Lightarrow.Mode
import System.Lightarrow.Sensation

slider  (_button :: Lens' (Sensation a) Bool)
        (_cursor :: Lens' (Sensation a) (Double, Double))
        (_x :: Lens' b (Double, Double, Double))
        (_d :: Lens' b (Double, Double))
        (_k :: Lens' b Double)
        idle
        hover
        sliding
                = chorus (do   rest (always (arrM stencil))
                               voice (toggle click sliding' released
                                        (pushdown above hover idle)))
    where   click       = mouseClick _button _cursor _x _d
            released    = falling (return . view _button)
            above       = mouseOver _cursor _x _d
            sliding'    = chorus (do   rest (always (arrM move))
                                       voice sliding)
            move        = slide _cursor (_x % _1) (_d % _1) _k
            stencil a   = do    inside  <- mouseOver _cursor _x _d a
                                tell (Any inside)    

slide _cursor _x _w _k a
    = do    x   <- gets (view _x)
            w   <- gets (view _w)
            let xL  = x - (w / 2)
                xR  = x + (w / 2)
                xC  = a ^. _cursor % _1
            modify (_k .~ min 1 (max 0 ((xC - xL) / (xR - xL))))


