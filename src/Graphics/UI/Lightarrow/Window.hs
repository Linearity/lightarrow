module Graphics.UI.Lightarrow.Window where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Bifunctor (second)
import Data.Lightarrow.SceneTransform
import Data.Lightarrow.SceneGraph
import Data.Tree
import Data.VectorSpace
import Data.MonadicStreamFunction hiding (embed, second)
import FRP.BearRiver hiding (embed, second)
import Graphics.UI.Lightarrow.Common
import Linear (V3(..))
import Optics
import Simulation.Lightarrow.Mode
import System.Lightarrow.Sensation
{-

A window is a region of the screen that encloses a GUI.

Windows each have a bus that GUI elements connect to. Each element adds a
signal (of type `Any`) to the bus stating whether or not the mouse cursor is
above it. The window runs these elements alongside its own mouse-sensitive
dragging behavior, also connected to the bus. The dragging only occurs when the
bus signal is `False`, that is, when the cursor is not above `Any` of the
elements.

-}
window  (_button :: Lens' (Sensation a) Bool)
        (_cursor :: Lens' (Sensation a) (Double, Double))
        (_x :: Lens' b (Double, Double, Double))
        (_d :: Lens' b (Double, Double))
        dragged
        idle
        items
            = newBus (toggle click dragged' released idle
                        `busMixM` items')
    where   items'          = bus (mapMode transform (runBus items))
            click           = proc a -> do
                                e       <- mouseClick _button _cursor _x _d -< a
                                inItem  <- constM (lift ask)                -< ()
                                returnA -< gate e (not (getAny inItem))
            dragged'        = chorus (do   rest (drag _cursor _x)
                                           voice dragged)
            released        = falling (return . view _button)
            transform sf    = proc (c, a) -> do
                                a'      <- arrM xfInput     -< a
                                ecbd    <- liftTransSF sf   -< (c, a')
                                f       <- constM xfDraw    -< ()
                                returnA -< second (\(c,b) -> (c,f b)) ecbd
            xfInput a       = do    c   <- xfCursor _x (a ^. _cursor)
                                    return (a & _cursor .~ negateVector c)
            xfDraw          = do    (x, y, z)   <- gets (view _x)
                                    return (\b -> Node (Frame (translate (V3 x y z))) [b])
