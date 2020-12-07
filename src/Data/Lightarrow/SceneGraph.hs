{-|

We arrange all of our output values in a graph that, like a mobile, defines
their relative positions in space. This is called a scene graph.

-}
module Data.Lightarrow.SceneGraph
    (   SceneGraph,
        SceneNode(..),
        runTree,
        _node,
        _kids,
        prune,
        cameraViews ) where

import Data.Graph
import Data.Maybe
import Data.Tree
import Data.Lightarrow.SceneTransform
import Linear
import Optics

type SceneGraph a b = Tree (SceneNode a b)

-- | A vertex of a scene graph.  There are several species.
data SceneNode a b
    = Camera                            -- ^ Viewpoint
    | Group                             -- ^ Group of nodes in the same coordinate frame
    | Frame (SceneTransform a)          -- ^ Local coordinate frame
    | Term (SceneTransform a -> b)      -- ^ Output based on a transformation

instance Show a => Show (SceneNode a b)
    where   show Camera     = "Camera"
            show Group      = "Group"
            show (Frame t)  = "Frame " ++ show t
            show (Term _)   = "Term <function>"

instance Semigroup (Tree (SceneNode a b)) where
    Node Group ks <> t  = Node Group (t:ks)
    t <> Node Group ks  = Node Group (t:ks)
    t1 <> t2            = Node Group [t1, t2]

instance Monoid (Tree (SceneNode a b)) where
    mempty = Node Group []

{-|

Given a 'Tree' of scene nodes, calculate the combined output values of
all its terms.

-}
runTree :: (Conjugate a, RealFloat a) =>
            ([b] -> b)                      -- ^ combine a list of outputs into one
                -> Tree (SceneNode a b)     -- ^ the scene graph
                -> b                        -- ^ the combined output
runTree cat t = foldTree f t []
    where   f (Frame t)  gs     = col (map (\g -> g . (t :)) gs)
            f (Term g)   gs     = col (g . foldr composeXf identityXf . reverse : gs)
            f _          gs     = col gs
            col gs ts           = cat (map ($ ts) gs)

-- | Read/write access to the node at the root of a scene graph
_node :: Lens' (Tree (SceneNode a b)) (SceneNode a b)
_node = lens rootLabel (\t n -> t { rootLabel = n })

-- | Read/write access to the subtrees of the root of a scene graph
_kids :: Lens' (Tree (SceneNode a b)) [Tree (SceneNode a b)]
_kids = lens subForest (\t ks -> t { subForest = ks })

{-|

Given a tree of scene nodes and a predicate on their transformations, remove
the branches of the tree whose descendants do not satisfy the predicate.

-}
prune :: (Conjugate a, RealFloat a)
            => (SceneTransform a -> Bool)           -- ^ predicate
                -> SceneTransform a                 -- ^ initial transformation
                -> Tree (SceneNode a b)             -- ^ unpruned scene graph
                -> Maybe (Tree (SceneNode a b))     -- ^ pruned scene graph
prune p xf (Node (Frame t) kids)
        | null kidsP    = Nothing
        | otherwise     = Just (Node (Frame t) kidsP)
    where   kidsP   = mapMaybe (prune p (xf `composeXf` t)) kids
prune p xf x = if p xf then Just x else Nothing
{-

Scene graphs can be huge. Often one encompasses a much larger scene than any
rendering actually depicts. A pruned tree provides more efficient
access to the relevant parts of the unpruned tree.

-}
{-|

The view transformations for each camera node in a given scene graph

-}
cameraViews :: Tree (SceneNode Double b) -> [SceneTransform Double]
cameraViews = camerasAux (Prelude.map CameraKey [0..]) . treeToGraph mkSceneNodeKey (CameraKey 0)

camerasAux ks (g, adjacency, vertex) = [getView identityXf p | p <- paths]
    where   getView xf (Node v [])
                            = case sceneNode v of
                                Frame xf2   -> inverseXf (composeXf xf2 xf)
                                _           -> inverseXf xf
            getView xf (Node v (k:_))
                            = case sceneNode v of
                                Frame xf2   -> getView (composeXf xf2 xf) k
                                _           -> getView xf k
            paths           = map (head . dfs gT . pure) cams  --search for root
            --undirected      = buildG (bounds g) (edges g ++ edges gT)
            gT              = transposeG g
            cams            = mapMaybe vertex ks    --valid camera vertices
            sceneNode v     = adjacency v ^. _1
{-

The identifying key for a scene node simply pairs a label, corresponding to the
constructor for a node, with a unique integer.

-}
data SceneNodeKey = FrameKey Int | GroupKey Int | CameraKey Int | TermKey Int
    deriving (Eq, Ord, Show)

sceneNodeKeyCode :: SceneNodeKey -> Int
sceneNodeKeyCode (FrameKey k)   = k
sceneNodeKeyCode (GroupKey k)   = k
sceneNodeKeyCode (CameraKey k)  = k
sceneNodeKeyCode (TermKey k)    = k
{-

The general representation using the structures of the |Data.Graph| module is
more cumbersome to use than the tree representation using |Data.Tree|, so
application code will likely use the latter. However, given a way to generate
vertex keys we can generate a general |Graph| from a |Tree|.

-}
treeToGraph ::  (Ord b, Show b)
                =>  (a -> b -> b)
                    -> b
                    -> Tree a
                    -> (Graph, Vertex -> (a, b, [b]), b -> Maybe Vertex)
treeToGraph mkKey k t = graphFromEdges (view _3 (getEdges k t))
    where   getEdges k0 (Node a c) = (k2, k1, (a, k1, ks) : concat eC)
                where   k1              = mkKey a k0
                        (k2, ks, eC)    = foldr col (k1, [], []) c
            col n (k1, ks, es) = (k3, k2 : ks, eN : es)
                where   (k3, k2, eN)    = getEdges k1 n
{-

One simple way to generate keys is just to use the appropriate labels and an
ever-increasing unique integer.

-}
mkSceneNodeKey :: SceneNode a b -> SceneNodeKey -> SceneNodeKey
mkSceneNodeKey  Group       key     = GroupKey  (sceneNodeKeyCode key + 1)
mkSceneNodeKey  (Frame _)   key     = FrameKey  (sceneNodeKeyCode key + 1)
mkSceneNodeKey  Camera      key     = CameraKey (sceneNodeKeyCode key + 1)
mkSceneNodeKey  (Term _)    key     = TermKey   (sceneNodeKeyCode key + 1)