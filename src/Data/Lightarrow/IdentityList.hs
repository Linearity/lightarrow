{-|

An identity list as used in /The Yampa Arcade/ by Antony Courtney and Henrik
Nilsson.  Because this module defines many of the same names as the Standard
Prelude and the modules of the "containers" package, it is meant to be
imported qualified.

-}
module Data.Lightarrow.IdentityList where

import qualified Data.IntMap.Strict as IM

-- | An identity list with nonnegative integers as identifiers
data IL a = IL { ilNextKey :: Int, ilTable :: IM.IntMap a }

instance Functor IL where
    fmap f (IL k t) = IL k (fmap f t)

instance Foldable IL where
    foldr f a0 (IL _ t) = foldr f a0 t

instance Traversable IL where
    traverse f (IL k t) = IL k <$> traverse f t

instance Semigroup (IL a) where
    IL k1 as <> IL k2 bs = IL (max k1 k2) (as <> bs)

instance Monoid (IL a) where
    mempty = IL 0 mempty

-- | IL with no elements
empty :: IL a
empty = IL 0 IM.empty

-- | Whether an IL has no elements
null :: IL a -> Bool
null (IL _ t) = IM.null t

-- | IL with the given elements
fromList :: [a] -> IL a
fromList = foldl (flip insert_) empty

-- | IL with the given elements and corresponding identifiers
fromAssocs :: [(Int, a)] -> IL a
fromAssocs as = IL (maximum (fmap fst as)) (IM.fromList as)

-- | Add a new element and return its identifier
insert :: a -> IL a -> (Int, IL a)
insert a (IL k t) = (k, IL (k + 1) (IM.insert k a t))

-- | A version of 'insert' that discards the new identifier
insert_ :: a -> IL a -> IL a
insert_ a (IL k t) = IL (k + 1) (IM.insert k a t)

-- | Remove an element, if it was present
delete :: Int -> IL a -> IL a
delete k (IL kN t) = IL kN (IM.delete k t)

{-|

An IL with the elements of two ILs. If both contain an element for a certain
identifier, the result contains the first argument's element.

-}
union :: IL a -> IL a -> IL a
union (IL k1 t1) (IL k2 t2) = IL (max k1 k2) (IM.union t1 t2)

{-|

Split an IL into two ILs; the given predicate is true of all the first's
elements and false of all the second's elements.

-}
partition :: (a -> Bool) -> IL a -> (IL a, IL a)
partition p (IL k t) = (IL k t1, IL k t2)
    where   (t1, t2) = IM.partition p t

-- | Transform all elements with a function of both the element and its identifier
map :: ((Int, a) -> b) -> IL a -> IL b
map f (IL k t) = IL k (IM.mapWithKey (curry f) t)

-- | Remove all elements not satisfying a predicate
filter :: (a -> Bool) -> IL a -> IL a
filter p (IL k t) = IL k (IM.filter p t)

-- | Fold with a right-associative operator over all elements and their
-- identifiers
keyFoldr :: ((Int, a) -> b -> b) -> b -> IL a -> b
keyFoldr f b0 (IL k t) = IM.foldrWithKey (curry f) b0 t

-- | Fold with a left-associative operator over all elements and their
-- identifiers
keyFoldl :: (b -> (Int, a) -> b) -> b -> IL a -> b
keyFoldl f b0 (IL k t) = IM.foldlWithKey (curry . f) b0 t

-- | The element corresponding to a given identifer, if it exists
lookup :: Int -> IL a -> Maybe a
lookup k (IL _ t) = IM.lookup k t

-- | The elements and identifiers of an IL
assocs :: IL a -> [(Int, a)]
assocs (IL _ t) = IM.assocs t

-- | The identifiers of an IL
keys :: IL a -> [Int]
keys (IL _ t) = IM.keys t

-- | The elements of an IL
elems :: IL a -> [a]
elems (IL _ t) = IM.elems t