module Graphics.UI.Lightarrow.Modulate where

import Control.Monad.Trans
import qualified Data.Bifunctor as BF
import FRP.BearRiver

import Graphics.UI.Lightarrow.Slider
import Graphics.UI.Lightarrow.Window
import Simulation.Lightarrow.Mode
import Debug.Trace
import Optics

modulateWithSliders n m
        = newBus
            (chorus
                (do     busVoice (mapMode (>>> firstOutputToBus) gui)
                        lift (mapMode (secondInputFromBus >>>) (noBus m))))
    where   gui             = mapMode ((>>> arr (BF.first (_1 %~ traceShowId))) . runW) guiWin
            runW            = runStateSF__ ((0, 0, 0), (50, n * 20))
            guiWin          = rectWindow (chorus (mapM_ (\k -> voice (mapMode (runS k) rectSlider)) [0..(n-1)]))
            runS k          = runStateSF__ (((0, ((n - 1) / 2 - k) * 20, 0), (40, 5)), 0)