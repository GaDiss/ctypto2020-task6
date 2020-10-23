module AVLTree
  (
    AVLTree (..)
  , height
  , label
  , getDigest
  , doLookup
  , doReplace
  , doInsert
  , doDelete
  , modify
  , modifyMany
  , fromList
  , newNode
  ) where

import Lib
import Hashing
import Data.ByteArray (ByteArrayAccess)
import qualified Data.Set as S

data AVLTree key value =
      MinLeaf
    | Leaf key value
    | LabelLeaf key Label
    | Node
        Height              -- ^ height of a tree
        key                 -- ^ key
        Label               -- ^ label
        (AVLTree key value) -- ^ left subtree
        (AVLTree key value) -- ^ right subtree

instance (Show key, Show value) => Show (AVLTree key value) where
  show MinLeaf = "L -inf NA"
  show (LabelLeaf k v) = "L* " ++ show k
  show (Leaf k v) = "L " ++ show k ++ " " ++ show v
  show (Node h k lbl l r) = "N "
                           ++ (show h) ++ " "
                           ++ (show k)  ++ " "
                           ++ "(" ++ (show l) ++ ") "
                           ++ "(" ++ (show r) ++ ")"

-- | getter for tree height
height :: (AVLTree key value) -> Height
height (Node h _ _ _ _) = h
height _                  = 0

-- | getter for tree label
label :: (ByteArrayAccess key, ByteArrayAccess value)
      => (AVLTree key value) -> Label
label MinLeaf = emptyLabel
label (LabelLeaf _ lbl) = lbl
label (Leaf k v) = labelLeaf k v
label (Node _ _ lbl _ _) = lbl


-- | returns digest of tree root
getDigest :: (ByteArrayAccess key, ByteArrayAccess value)
          => (AVLTree key value)
          -> RootDigest
getDigest tree = RootDigest (label tree) (height tree)

-- | Tries to find a value by given key  : O(log(n))
-- returns Just value if found, Nothing if not
--     and a new tree
doLookup :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => key
         -> (AVLTree key value)
         -> (Maybe value, S.Set key, AVLTree key value)
doLookup k = modify $ Lookup k

-- | Tries to replace a value in a leaf by given key  : O(log(n))
-- returns Just new value if successful, Nothing if not
--     and a new tree
doReplace :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
          => key
          -> value
          -> (AVLTree key value)
          -> (Maybe value, S.Set key, AVLTree key value)
doReplace k v = modify $ Replace k v

-- | Tries to insert a leaf by given key  : O(log(n))
-- returns Just inserted value if successful, Nothing if not
--     and a new tree
doInsert :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => key
         -> value
         -> (AVLTree key value)
         -> (Maybe value, S.Set key, AVLTree key value)
doInsert k v = modify $ Insert k v

-- | Tries to delete a leaf by given key  : O(log(n))
-- returns Just deleted value if successful, Nothing if not
--     and a new tree
doDelete :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => key
         -> (AVLTree key value)
         -> (Maybe value, S.Set key, AVLTree key value)
doDelete k = modify $ Delete k

-- | Applies single operation to a tree  : O(log(n))
-- returns a result of given operation (specified below)
--     and a new tree
modify :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
       => (Operation key value)                     -- ^ operation, applied to old tree
       -> (AVLTree key value)                       -- ^ old tree
       -> (Maybe value, S.Set key, AVLTree key value) -- ^ (return value, visited keys, new tree)
modify op (Node h nk nl l r) =
  case compare k nk of
    LT -> (maybeLVal, (S.union lBalS lSet), lFinT)
    GT -> (maybeRVal, (S.union rBalS rSet), rFinT)
    EQ -> case op of
      (Delete k) -> case r of
                    Leaf kk vv -> (Just vv, (S.singleton nk), l)
                    _          -> (replacedValue, (S.insert nk (S.union adBalS delSet)), adFinT)
      _          -> (maybeEqVal, (S.union mBalS mSet), mFinT)
  where
    this = (Node h nk nl l r)
    k = getKey op
    lOld = l
    rOld = r
    (maybeLVal, lSet, lNew) = modify op l
    (lBalS, lFinT) = balance (newNode nk lNew rOld)
    (maybeRVal, rSet, rNew) = modify op r
    (rBalS, rFinT) = balance (newNode nk lOld rNew)
    (maybeEqVal, mSet, rNewEq) = modifyMin op r
    (mBalS, mFinT) = balance (newNode nk lOld rNewEq)
    (delKey, delLSet, delLeaf, lNewDel) = deleteRight l
    (replacedV, delRSet, changedTree) = (replaceLeftLeaf delLeaf r)
    (replacedValue, delSet, afterDelete) =
      case l of
        MinLeaf                   -> replaceLeftLeaf MinLeaf r
        (Leaf leftK leftV)        -> replaceLeftLeaf (Leaf leftK leftV) r
        (LabelLeaf lleftK lleftV) -> replaceLeftLeaf (LabelLeaf lleftK lleftV) r
        _                         -> (replacedV, (S.union delLSet delRSet), newNode delKey lNewDel changedTree)
    (adBalS, adFinT) = balance afterDelete
modify (Insert k v) MinLeaf = (Just v, S.empty, newNode k MinLeaf (Leaf k v))
modify _ MinLeaf = (Nothing, S.empty, MinLeaf)
modify _ (LabelLeaf k lbl) = (Nothing, S.empty, LabelLeaf k lbl)
modify op (Leaf lk lv)
  | k == lk = case op of
                (Lookup k)    -> (Just lv, S.empty, (Leaf lk lv))
                (Replace k v) -> (Just v, S.empty, (Leaf k v))
                (Insert k v)  -> (Nothing, S.empty, (Leaf lk lv))
                _             -> error "tree is incorrect"
  | k < lk = (Nothing, S.empty, (Leaf lk lv))
  | k > lk = case op of
               (Insert k v) -> (Just v, S.empty, newNode k (Leaf lk lv) (Leaf k v))
               _            -> (Nothing, S.empty, (Leaf lk lv))
  where
    k = getKey op

-- | Applies given operations to a given tree using modify function   : O(|ops| * log(n))
-- returns results for operations and resulting tree
modifyMany :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
           => [Operation key value]                         -- ^ operations, applied to old tree
           -> (AVLTree key value)                           -- ^ old tree
           -> ([Maybe value], S.Set key, AVLTree key value) -- ^ (return value, visited, new tree)
modifyMany ops oldT = foldl ansCollector ([], S.empty, oldT) ops

fromList :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => [(key, value)] -> AVLTree key value
fromList list = foldr (\(k, v) t -> third3 $ doInsert k v t) MinLeaf list

-- | creates a new Node from key and its children
newNode :: (ByteArrayAccess key, ByteArrayAccess value)
        => key -> (AVLTree key value) -> (AVLTree key value) -> (AVLTree key value)
newNode k l r = (Node h k lbl l r)
  where
    h = (+1) $ max (height l) (height r)
    lbl = labelCombine h (label l) (label r)

-- | AVL Tree Left rotation
rotateL :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
        => (AVLTree key value) -> (S.Set key, AVLTree key value)
rotateL (Node _ ka _ l (Node _ kb _ c r))
      = (S.fromList [ka, kb], newNode kb (newNode ka l c) r)

-- | AVL Tree Left Right rotation
rotateLR :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => (AVLTree key value) -> (S.Set key, AVLTree key value)
rotateLR (Node _ ka _ l (Node _ kb _ (Node _ kc _ m n) r))
       = (S.fromList [ka, kb, kc], newNode kc (newNode ka l m) (newNode kb n r))

-- | AVL Tree Right rotation
rotateR :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
        => (AVLTree key value) -> (S.Set key, AVLTree key value)
rotateR (Node _ ka _ (Node _ kb _ l c) r)
      = (S.fromList [ka, kb], newNode kb l (newNode ka c r))

-- | AVL Tree Right Left rotation
rotateRL :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => (AVLTree key value) -> (S.Set key, AVLTree key value)
rotateRL (Node _ ka _ (Node _ kb _ l (Node _ kc _ m n)) r)
       = (S.fromList [ka, kb, kc], newNode kc (newNode kb l m) (newNode ka n r))

-- | compares heights of kids of a given node
compareKidsHeight :: (AVLTree key value) -> Ordering
compareKidsHeight (Node h ka la l r) = compare (height l) (height r)
compareKidsHeight _ = EQ

-- | AVL balancing operation
balance :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
        => (AVLTree key value) -> (S.Set key, AVLTree key value)
balance (Node h ka la l r) =
  case (height r) - (height l) of
    2 -> case compareKidsHeight r of
           GT -> rotateLR (Node h ka la l r)
           _  -> rotateL  (Node h ka la l r)
    -2 -> case compareKidsHeight l of
           LT -> rotateRL (Node h ka la l r)
           _  -> rotateR  (Node h ka la l r)
    _ -> (S.singleton ka, Node h ka la l r)
balance leaf = (S.empty, leaf)

-- | helps with modifying
modifyMin :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
          => (Operation key value)                       -- ^ operation, applied to old tree
          -> (AVLTree key value)                         -- ^ old tree
          -> (Maybe value, S.Set key, AVLTree key value) -- ^ (return value, visited, new tree)
modifyMin op (Node _ k _ l r) = (mv, (S.union balS set), tree)
  where
    (balS, tree) = balance (newNode k mp r)
    (mv, set, mp) = modifyMin op l
modifyMin op leaf = modify op leaf

-- | helps with deletion
replaceLeftLeaf :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
                => (AVLTree key value)                         -- ^ inserting instead of leaf
                -> (AVLTree key value)                         -- ^ tree where new leaf is inserted
                -> (Maybe value, S.Set key, AVLTree key value) -- ^ (deleted value, visited, resulting tree)
replaceLeftLeaf leaf (Node _ k _ l r) = (v, (S.insert k set), newNode k left r)
  where (v, set, left) = (replaceLeftLeaf leaf l)
replaceLeftLeaf leaf (Leaf _ v)         = (Just v, S.empty, leaf)
replaceLeftLeaf leaf MinLeaf            = (Nothing, S.empty, leaf)

-- | helps with deletion
deleteRight :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
            => (AVLTree key value)    -- ^ initial tree
            -> ( key                  -- ^ key of deleted leaf
               , S.Set key              -- ^ S.Set of visited keys
               , (AVLTree key value)  -- ^ deleted leaf
               , (AVLTree key value)) -- ^ new tree
deleteRight (Node _ k _ l r) =
  case r of
    (Leaf rKey rVal)      -> (rKey, (S.singleton k), (Leaf rKey rVal), l)
    (LabelLeaf rKey lbl) -> (rKey, (S.singleton k), (LabelLeaf rKey lbl), l)
    _                     -> (delKey, (S.union balS retS), delLeaf, tree)
  where
    (balS, tree) = balance (newNode k l delRight)
    (delKey, retS, delLeaf, delRight) = deleteRight r

-- | helps to collect answers in modifyMany
ansCollector :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
             => ([Maybe value], S.Set key, AVLTree key value)
             -> (Operation key value)
             -> ([Maybe value], S.Set key, AVLTree key value)
ansCollector (res, set, t) op = ((res ++ [resN]), (S.union set setN), tN)
  where (resN, setN, tN) = modify op t
