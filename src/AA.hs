{-|
Module      : AA
Description : Map library based on AA trees.
License     : GPL-3
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve
Stability   : experimental
Portability : POSIX
-}

module AA 
    ( AA
    , empty 
    , isEmpty
    , insert
    , insertWithKey
    , insertWith
    , lookup
    , delete 
    , member
    , fromList
    , toList
    )
    where

import Prelude hiding (lookup)
import Control.Monad  ( when )
import Data.Bifunctor ( Bifunctor(first) )
import Data.Foldable  ( traverse_ )

---------------------------------
-- Types                        |
---------------------------------

-- | Definition of an AA Tree specialized to hold `(Key,Val)` pairs.
data AA k a 
    = Empty
    | Node { lvl :: Int
    , key :: k
    , val :: a
    , lAA :: AA k a
    , rAA :: AA k a
    } deriving (Eq, Show)
    
---------------------------------
-- Instances                    |
---------------------------------

instance Functor (AA k) where
    fmap f Empty                    = Empty 
    fmap f (Node l k v lNode rNode) = Node l k (f v) (fmap f lNode) (fmap f rNode)

instance Foldable (AA k) where
    foldr _ acc Empty = acc
    foldr f acc (Node _ _ v left right) = foldr f acc' left 
        where
            acc' = f v $ foldr f acc right  

-- | A more general fold that takes into consideration the key.
foldrWithKey :: Ord k => (k -> a -> b -> b) -> b -> AA k a -> b
foldrWithKey _ acc Empty = acc
foldrWithKey f acc (Node _ k v left right) = foldrWithKey f acc' left 
        where
            acc' = f k v $ foldrWithKey f acc right  

---------------------------------
-- Builder                      |
---------------------------------

-- | Builds an empty AA Tree.
empty :: Ord k => AA k a
empty = Empty

-- | Builds a tree from a list of `(Key,Value)` pairs.
fromList :: Ord k => [(k,v)] -> AA k v
fromList = foldr (\(k,v) t -> insert k v t) empty

---------------------------------
-- Predicates                   |
---------------------------------

-- | Checks whether we have an empty tree
isEmpty :: AA k a -> Bool
-- foldable goes BRRRRRRRRRRRRRRRRRRRRRRRRRR
isEmpty = null

-- | Checks wheter a `Key` belongs to the tree. Useful if Tree is treated as a set.
member :: Ord k => k -> AA k v -> Bool
member k t = case lookup k t of 
  Nothing -> False 
  Just _  -> True

---------------------------------
-- Functions                    |
---------------------------------

-- | Inserts a pair `(Key,Value)` into the tree using the provided function to combine the
-- contents in case of collision.
insertWithKey :: Ord k => k -> a -> (k -> a -> a -> a) -> AA k a -> AA k a
insertWithKey k v _ Empty = Node 1 k v Empty Empty 
insertWithKey k v f t@Node{key=_key,val=_val,lAA=l,rAA=r}
    | k < _key  = split . skew $ t{lAA =insertWithKey k v f l}
    | k > _key  = split . skew $ t{rAA =insertWithKey k v f r}
    | otherwise = t{val= f k v _val}

-- |  Inserts a pair `(Key,Value)` into the tree using the provided function to combine the
-- contents in case of collision. 
insertWith :: Ord k => k -> a -> (a -> a -> a) -> AA k a -> AA k a
insertWith k v f = insertWithKey k v (\_ v v' -> f v v') 

-- |  Inserts a pair `(Key,Value)` into the tree replacing the contents in case of collision.
insert :: Ord k => k -> a -> AA k a -> AA k a
insert k v = insertWith k v const

-- | Deletes a node `Key` from the tree.
delete :: Ord k => k -> AA k a -> AA k a 
-- I'm so verry sorry about the code, it was too imperative...
-- maybe with lenses it would have been more elegant?
delete _ Empty = Empty
delete k n@Node {lvl=_lvl, key=_key, val=_val, lAA=_lAA, rAA=_rAA}
    | k > _key = g n{rAA= delete k _rAA}
    | k < _key = g n{lAA= delete k _lAA}
    | isLeaf n = getRight n
    | isEmpty $ getLeft n = 
                let l@Node{key=k',val=v} = successor n
                    r  = delete k' $ getRight n
                    n' = n{key=k',val=v,rAA=r}
                in g n'
    | otherwise = 
                let l@Node{key=k',val=v} = predecessor n
                    left = delete k' $ getLeft n
                    n'   = n{key=k',val=v,lAA=left}
                in g n'
    where
        
        infixr 9 |>
        (|>) :: (a -> b) -> (b -> c) -> a -> c
        (|>) = flip (.)

        g :: Ord k => AA k a -> AA k a 
        g  
            = decreaseLevel 
            |> skew 
            |> (\n@Node{rAA=_rAA} -> n{rAA=skew _rAA})
            |> h
            |> split 
            |> (\n@Node{rAA=_rAA} -> n{rAA=split _rAA})
        
        h n@Node{rAA=Empty} = n
        h n@Node{rAA=n'@Node {rAA=_rAA}} = n{rAA=n'{rAA=skew _rAA}}
        h n = n
        
        
-- | Safe accesor.
lookup :: Ord k => k -> AA k v -> Maybe v
lookup _ Empty = Nothing 
lookup k Node{key=_key,val=_val,lAA=_lAA,rAA=_rAA}
    | k < _key  = lookup k _lAA
    | k > _key  = lookup k _rAA
    | otherwise = Just _val 

-- | Catamorphs the tree into a list of `(Key,Value)` pairs.
toList :: Ord k => AA k v -> [(k,v)]
toList = foldrWithKey (\k v acc -> (k,v):acc) []


---------------------------------
-- Internal                     |
---------------------------------

-- | Checks whether a node is a leaf.
isLeaf :: AA k a -> Bool
isLeaf Node {lvl=1} = True 
isLeaf _ = False

-- | Tries to access the right child of a node.
getRight :: AA k a -> AA k a
getRight Empty = Empty
getRight Node {rAA=_rAA} = _rAA 

-- | Tries to access the left child of a node.
getLeft :: AA k a -> AA k a
getLeft Empty = Empty
getLeft Node {lAA=_lAA} = _lAA 

-- | Gets the successor of a node.
successor :: AA k a -> AA k a
successor Empty = Empty
successor Node{rAA=Empty} = Empty
successor Node{rAA=n} = getMin n 
    where
        getMin Empty = Empty
        getMin n@Node {lAA=Empty} = n{lvl=1}
        getMin n = getMin $ getLeft n 

-- | Gets the predecessor of a node.
predecessor :: AA k a -> AA k a
predecessor Empty = Empty
predecessor Node{lAA=Empty} = Empty
predecessor Node{lAA=n} = getMax n 
    where
        getMax Empty = Empty
        getMax n@Node {rAA=Empty} = n{lvl=1}
        getMax n = getMax $ getRight n 

-- | Insertion at a tree level.
modifyWithCrumbs :: (AA k a -> AA k a) -> Crumbs -> AA k a -> AA k a
modifyWithCrumbs f [] t = f t
modifyWithCrumbs f (L:cs) t@Node{lAA=_lAA} = t{lAA = modifyWithCrumbs f cs _lAA}
modifyWithCrumbs f (R:cs) t@Node{rAA=_rAA} = t{rAA = modifyWithCrumbs f cs _rAA}
modifyWithCrumbs _ _ Empty = Empty

modifyLeft, modifyRight :: (AA k a -> AA k a) -> AA k a -> AA k a 

modifyLeft  f = modifyWithCrumbs f [L]
modifyRight f = modifyWithCrumbs f [R]

setLeft, setRight :: AA k a -> AA k a -> AA k a 

setLeft l = modifyLeft (const l)
setRight l = modifyRight (const l)

-- | Skew operation.
skew :: Ord k => AA k v -> AA k v
skew Empty = Empty
skew t@Node{lAA=Empty} = t
skew t@(Node lvl _ _ l@Node{lvl=lvlL, rAA=rAAL} _) 
    | lvlL == lvl = l{rAA = t{lAA=rAAL}}
    | otherwise   = t 

-- | Split  operation.
split :: Ord k => AA k v -> AA k v 
split Empty = Empty
split t@Node{rAA=Empty} = t
split t@Node{rAA=Node{rAA=Empty}} = t
split t@(Node lvl key val lAA r@Node{lvl=lvlR, lAA=lAAR,rAA=Node{lvl=lvlRR}}) 
    | lvl <= lvlRR = r{lvl=lvlR + 1, lAA=t{rAA=lAAR}}
    | otherwise  = t

-- | Decrease level operation.
decreaseLevel :: Ord k => AA k v -> AA k v
decreaseLevel Empty = Empty
decreaseLevel t@Node{lvl=_lvl,lAA=l,rAA=r}
    | shouldBe < _lvl = t{lvl=shouldBe,rAA=newRight} 
    | otherwise       = t 
    where
        getLevel Empty = 0
        getLevel Node {lvl=_lvl} = _lvl 

        newRight = if shouldBe < getLevel r then setLevel shouldBe r else r

        shouldBe = min (getLevel l) (getLevel r) + 1

        setLevel _ Empty = Empty
        setLevel n t     = t{lvl = n}

---------------------------------
-- Aux Types?                   |
---------------------------------

data Direction = L | R deriving (Eq,Show)

type Crumbs = [Direction]

data InvariantError = BadLeafLevel | BadLeftLevel | BadRightLevel | BadGrandLevel | OneChild deriving (Eq,Show)

type VTree k a =  Either (Crumbs,InvariantError) (AA k a)


---------------------------------
-- Auxiliar func Invariant      |
---------------------------------

combineVTree :: Direction -> VTree k v -> VTree k v
combineVTree _ (Right v) = Right v
combineVTree dir (Left (lista,err)) = Left ( [dir]++lista, err )

---------------------------------
-- Invariant                    |
---------------------------------

checkInvariant :: Ord k => AA k v -> VTree k v
checkInvariant nodo = checkInvariant' nodo Empty

checkInvariant' :: Ord k => AA k v -> AA k v -> VTree k v
checkInvariant' Empty grandP = Right $ Empty
-- Casos lvl = 1
checkInvariant' (Node lvl key val Empty Empty) _ 
    | lvl == 1 = Right $ (Node 1 key val Empty Empty)
    | otherwise = Left $ ([],OneChild) 
-- lvl>1
checkInvariant' (Node l k v lNode rNode) grandP
    | l>1 && ( (isEmpty lNode) || (isEmpty rNode) )
        = Left $ ([],OneChild) 
    | not ( isEmpty lNode ) && ( lvl lNode ) /= (l-1) 
        = Left $ ([],BadLeftLevel) 
    | not ( isEmpty rNode ) && ( lvl rNode ) > l 
        = Left $ ([],BadRightLevel)
    | not ( isEmpty grandP ) && ( (lvl grandP) <= (lvl rNode) ) 
        = Left $ ([R], BadGrandLevel)
    | otherwise = do 
        lNode2 <- combineVTree L $ checkInvariant' lNode cuNode 
        rNode2 <- combineVTree R $ checkInvariant' rNode cuNode 
        Right cuNode 
    where cuNode = Node l k v lNode rNode


---------------------------------------
-- Idea salvaje de dani
---------------------------------------

---------------------------
-- A falta de Monad.Error |
---------------------------

throwError :: e -> Either e a
throwError = Left 

throwEWhen :: Bool -> e ->  Either e ()
throwEWhen b e = when b (throwError e) 


-- Todos los posibles casos que puede dar error
badNode, badLeaf, badLeftLevel, badRightLevel, badGrandLevel, badOneChild :: AA k v ->  Either InvariantError ()

-- generic programming go brrrrr, ahora se puede reemplazar perfectamente por cualquier Monad.Error
-- o mejor aun, cualquier applicative cuya semantica capte errores.
checkInvariant'' :: AA k v -> VTree k v
checkInvariant'' Empty              = pure Empty
checkInvariant'' t@(Node n k v l r) 
    =  first (\e -> ([],e)) (badNode t) 
    >> first (first (L:)) (checkInvariant'' l)
    >> first (first (R:)) (checkInvariant'' r)
    >> pure t


badNode t = traverse_ ($ t) [badLeaf, badLeftLevel, badRightLevel, badGrandLevel, badOneChild] 

badLeaf t@Node{lvl=n, lAA=Empty, rAA=Empty} = throwEWhen (n /= 1) BadLeafLevel 
badLeaf t = pure ()

badLeftLevel t@Node{lvl=n,lAA=Node{lvl=ln}} = throwEWhen (ln + 1 /= n) BadLeftLevel 
badLeftLevel t = pure ()

badRightLevel t@Node {lvl=n,rAA=Node{lvl=rn}} = throwEWhen (rn + 1 /= n && rn /= n) BadRightLevel 
badRightLevel t = pure ()

badGrandLevel t@Node{lvl=n,rAA=Node{rAA=Node{lvl=rrn}}} = throwEWhen (n <= rrn) BadGrandLevel 
badGrandLevel t = pure ()

badOneChild t@Node{lvl=n, lAA=Empty} = throwEWhen (n > 1) OneChild 
badOneChild t@Node{lvl=n, rAA=Empty} = throwEWhen (n > 1) OneChild 
badOneChild t = pure ()

