module Graphics.UI.Lightarrow.Slider (slider, rectSlider) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Lightarrow.Color
import Data.Lightarrow.SceneGraph
import Data.Lightarrow.SceneTransform
import Data.MonadicStreamFunction hiding (embed)
import Graphics.Lightarrow.Rectangle
import Graphics.UI.Lightarrow.Common
import Linear
import Optics hiding (over)
import Simulation.Lightarrow.Mode
import System.Lightarrow.Mouse
import System.Lightarrow.Actuation
import System.Lightarrow.Sensation

-- | A horizontal slider element
slider :: (MonadState s m, MonadWriter Any m, MonadFix m, Monoid b) =>
            Lens' a Bool                                -- ^ button state
                -> Lens' a (Double, Double)             -- ^ cursor position
                -> Lens' s (Double, Double, Double)     -- ^ slider position
                -> Lens' s (Double, Double)             -- ^ slider dimensions
                -> Lens' s Double                       -- ^ slider knob position
                -> Mode a b m ()                        -- ^ idle mode
                -> Mode a b m ()                        -- ^ hover mode
                -> Mode a b m c                         -- ^ sliding mode
                -> Mode a b m ()                        -- ^ the slider
slider  _button _cursor _x _d _k idle hover sliding
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


