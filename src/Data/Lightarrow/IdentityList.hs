module Data.Lightarrow.IdentityList where

import qualified Data.IntMap.Strict as IM

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

empty :: IL a
empty = IL 0 IM.empty

null :: IL a -> Bool
null (IL _ t) = IM.null t

fromList :: [a] -> IL a
fromList as = foldl (flip insert_) empty as

fromAssocs :: [(Int, a)] -> IL a
fromAssocs as = IL (maximum (fmap fst as)) (IM.fromList as)

insert :: a -> IL a -> (Int, IL a)
insert a (IL k t) = (k, IL (k + 1) (IM.insert k a t))

insert_ :: a -> IL a -> IL a
insert_ a (IL k t) = IL (k + 1) (IM.insert k a t)

delete :: Int -> IL a -> IL a
delete k (IL kN t) = IL kN (IM.delete k t)

union :: IL a -> IL a -> IL a
union (IL k1 t1) (IL k2 t2) = IL (max k1 k2) (IM.union t1 t2)

partition :: (a -> Bool) -> IL a -> (IL a, IL a)
partition p (IL k t) = (IL k t1, IL k t2)
    where   (t1, t2) = IM.partition p t

map :: ((Int, a) -> b) -> IL a -> IL b
map f (IL k t) = IL k (IM.mapWithKey (curry f) t)

filter :: (a -> Bool) -> IL a -> IL a
filter p (IL k t) = IL k (IM.filter p t)

lookup :: Int -> IL a -> Maybe a
lookup k (IL _ t) = IM.lookup k t

assocs :: IL a -> [(Int, a)]
assocs (IL _ t) = IM.assocs t

keys :: IL a -> [Int]
keys (IL _ t) = IM.keys t

elems :: IL a -> [a]
elems (IL _ t) = IM.elems t
