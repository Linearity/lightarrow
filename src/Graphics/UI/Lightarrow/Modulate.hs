module Graphics.UI.Lightarrow.Modulate where

import Control.Monad.Trans
import qualified Data.Bifunctor as BF
import FRP.BearRiver
import FRP.BearRiver.Monad
import Optics

import Graphics.UI.Lightarrow.Slider
import Graphics.UI.Lightarrow.Window
import Simulation.Lightarrow.Task

modulateWithSliders n m
        = newBus
            (chorus
                (do     busVoice (mapTask (>>> firstOutputToBus) gui)
                        lift (mapTask (secondInputFromBus >>>) (noBus m))))
    where   gui             = mapTask runW guiWin
            runW            = execStateSF ((0, 0, 0), (50, n * 20))
            guiWin          = rectWindow (chorus (mapM_ (\k -> voice (mapTask (runS k) rectSlider)) [0..(n-1)]))
            runS k          = execStateSF (((0, ((n - 1) / 2 - k) * 20, 0), (40, 5)), 0)