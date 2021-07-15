{-|

We arrange all of our output values in a graph that, like a mobile, defines
their relative positions in space. This is called a scene graph.

-}
module Data.Lightarrow.SceneGraph
    (   module Data.Tree,
        SceneGraph,
        SceneNode(..),
        frame,
        runTree,
        _node,
        _kids,
        cameraViews ) where

import Data.Graph
import Data.Maybe
import Data.Monoid
import Data.Tree
import Data.Lightarrow.SceneTransform
import Linear
import Optics

type SceneGraph a b = Tree (SceneNode a b)

-- | A vertex of a scene graph.  There are several species.
data SceneNode a b
    = Camera                            -- ^ viewpoint
    | Group                             -- ^ group of nodes in the same coordinate frame
    | Frame !(SceneTransform a)         -- ^ local coordinate frame
    | Term (SceneTransform a -> b)      -- ^ output based on a transformation

instance Show a => Show (SceneNode a b)
    where   show Camera     = "Camera"
            show Group      = "Group"
            show (Frame t)  = "Frame " ++ show t
            show (Term _)   = "Term <function>"

instance Eq a => Semigroup (SceneGraph a b) where
    Node Group ks1 <> Node Group ks2
                        = Node Group (ks1 <> ks2)
    Node Group ks <> t  = Node Group (t:ks)
    t <> Node Group ks  = Node Group (t:ks)
    Node (Frame t0) ks1 <> Node (Frame t1) ks2
        | t0 == t1      = Node (Frame t0) (ks1 <> ks2)
    t1 <> t2            = Node Group [t1, t2]

instance Eq a => Monoid (SceneGraph a b) where
    mempty = Node Group []

-- | Add a coordinate transformation above the root of a scene graph
frame :: SceneTransform a -> SceneGraph a b -> SceneGraph a b
frame xf = Node (Frame xf) . (: [])

{-|

Given a 'Tree' of scene nodes, calculate the combined output values of
all its terms.

-}
runTree :: (Conjugate a, RealFloat a, Monoid b) =>
                (SceneTransform a -> Bool)      -- ^ criterion for included terms
                -> SceneGraph a b               -- ^ the scene graph
                -> b                            -- ^ the combined output
runTree keep t = run identityXf [t] id
    where
        run !_ [] c
            = c mempty
        run xf (Node (Term f) _ : ts) c
            | keep xf   = run xf ts (c . (f xf <>))
            | otherwise = run xf ts c
        run !xf0 (Node (Frame xf) ks : ts) c
            = run (xf0 `composeXf` xf) ks (\b -> run xf0 ts (c . (b <>)))
        run !xf (Node Group ks : ts) c
            = run xf ks (\b -> run xf ts (c . (b <>)))
        run !xf (_ : ts) c
            = run xf ts c

-- | Read/write access to the node at the root of a scene graph
_node :: Lens' (SceneGraph a b) (SceneNode a b)
_node = lens rootLabel (\t n -> t { rootLabel = n })

-- | Read/write access to the subtrees of the root of a scene graph
_kids :: Lens' (SceneGraph a b) [SceneGraph a b]
_kids = lens subForest (\t ks -> t { subForest = ks })

-- | The view transformations for each camera node in a given scene graph
cameraViews :: Tree (SceneNode Double b) -> [SceneTransform Double]
cameraViews = run identityXf
    where
        run !xf (Node (Term f) _) = []
        run !xf0 (Node (Frame xf) ks) = concatMap (run (xf0 `composeXf` xf)) ks
        run !xf (Node Group ks) = concatMap (run xf) ks
        run !xf (Node Camera _) = [inverseXf xf]