module AVLTree
  (
    AVLTree (..)
  , notVisited
  , doLookup
  , doReplace
  , doInsert
  , doDelete
  , modify
  , modifyMany
  ) where

import Lib
import Hashing
import Data.ByteArray (ByteArrayAccess)

data AVLTree key value =
      MinLeaf
    | Leaf key value
    | Node
        Height              -- ^ height of a tree
        Bool                -- ^ visited
        key                 -- ^ key
        Label               -- ^ label
        (AVLTree key value) -- ^ left subtree
        (AVLTree key value) -- ^ right subtree

instance (Show key, Show value) => Show (AVLTree key value) where
  show MinLeaf = "L -inf NA"
  show (Leaf k v) = "L " ++ show k ++ " " ++ show v
  show (Node h b k lbl l r) = "N "
                           ++ (show h) ++ " "
                           ++ (show b) ++ " "
                           ++ (show k)  ++ " "
                           ++ "(" ++ (show l) ++ ") "
                           ++ "(" ++ (show r) ++ ")"

-- | getter for tree height
height :: (AVLTree key value) -> Height
height (Node h _ _ _ _ _) = h
height _                  = 0

-- | getter for tree label
label :: (ByteArrayAccess key, ByteArrayAccess value)
      => (AVLTree key value) -> Label
label MinLeaf = emptyLabel
label (Leaf k v) = labelLeaf k v
label (Node _ _ _ l _ _) = l

-- | resets visited marks in all of tree : O(n)
notVisited :: (AVLTree key value) -> (AVLTree key value)
notVisited (Node h _ k lbl l r) = (Node h False k lbl (notVisited l) (notVisited r))
notVisited leaf = leaf

-- | Tries to find a value by given key  : O(log(n))
-- returns Just value if found, Nothing if not
--     and a new tree
doLookup :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => key
         -> (AVLTree key value)
         -> (Maybe value, AVLTree key value)
doLookup k = modify $ Lookup k

-- | Tries to replace a value in a leaf by given key  : O(log(n))
-- returns Just new value if found, Nothing if not
--     and a new tree
doReplace :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
          => key
          -> value
          -> (AVLTree key value)
          -> (Maybe value, AVLTree key value)
doReplace k v = modify $ Replace k v

-- | Tries to insert a leaf by given key  : O(log(n))
-- returns Just inserted value if found, Nothing if not
--     and a new tree
doInsert :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => key
         -> value
         -> (AVLTree key value)
         -> (Maybe value, AVLTree key value)
doInsert k v = modify $ Insert k v

-- | Tries to delete a leaf by given key  : O(log(n))
-- returns Just deleted value if found, Nothing if not
--     and a new tree
doDelete :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => key
         -> (AVLTree key value)
         -> (Maybe value, AVLTree key value)
doDelete k = modify $ Delete k

-- | Applies single operation to a tree  : O(log(n))
-- returns a result of given operation (specified below)
--     and a new tree
modify :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
       => (Operation key value)            -- ^ operation, applied to old tree
       -> (AVLTree key value)              -- ^ old tree
       -> (Maybe value, AVLTree key value) -- ^ (return value, new tree)
modify op (Node h _ nk nl l r) =
  case compare k nk of
    LT -> (maybeLVal, balance (newNode nk lNew rOld))
    GT -> (maybeRVal, balance (newNode nk lOld rNew))
    EQ -> case op of
      (Delete k) -> case r of
                    Leaf kk vv -> (Just vv, l)
                    _ -> (replacedValue, balance afterDelete)
      _          -> (maybeEqVal, balance (newNode nk lOld rNewEq))
  where
    this = (Node h True nk nl l r)
    k = getKey op
    lOld = l
    rOld = r
    (maybeLVal, lNew) = modify op l
    (maybeRVal, rNew) = modify op r
    (maybeEqVal, rNewEq) = modifyMin op r
    (delKey, delLeaf, lNewDel) = deleteRight l
    (replacedV, changedTree) = (replaceLeftLeaf delLeaf r)
    (replacedValue, afterDelete) =
      case l of
        MinLeaf            -> replaceLeftLeaf MinLeaf r
        (Leaf leftK leftV) -> replaceLeftLeaf (Leaf leftK leftV) r
        _                  -> (replacedV, newNode delKey lNewDel changedTree)
modify (Insert k v) MinLeaf = (Just v, newNode k MinLeaf (Leaf k v))
modify _ MinLeaf = (Nothing, MinLeaf)
modify op (Leaf lk lv)
  | k == lk = case op of
                (Lookup k)    -> (Just lv, (Leaf lk lv))
                (Replace k v) -> (Just v, (Leaf k v))
                (Insert k v)  -> (Nothing, (Leaf lk lv))
                _             -> error "tree is incorrect"
  | k < lk = (Nothing, (Leaf lk lv))
  | k > lk = case op of
               (Insert k v) -> (Just v, newNode k (Leaf lk lv) (Leaf k v))
               _            -> (Nothing, (Leaf lk lv))
  where
    k = getKey op

-- | Applies given operations to a given tree using modify function   : O(|ops| * log(n))
-- returns results for operations and resulting tree
modifyMany :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
           => [Operation key value]              -- ^ operations, applied to old tree
           -> (AVLTree key value)                -- ^ old tree
           -> ([Maybe value], AVLTree key value) -- ^ (return value, new tree)
modifyMany ops oldT = foldl ansCollector ([], oldT) ops

-- | creates a new Node
newNode :: (ByteArrayAccess key, ByteArrayAccess value)
        => key -> (AVLTree key value) -> (AVLTree key value) -> (AVLTree key value)
newNode k l r = (Node h True k lbl l r)
  where
    h = (+1) $ max (height l) (height r)
    lbl = labelCombine h (label l) (label r)

-- | AVL Tree Left rotation
rotateL :: (ByteArrayAccess key, ByteArrayAccess value)
        => (AVLTree key value) -> (AVLTree key value)
rotateL (Node _ _ ka _ l (Node _ _ kb _ c r))
      = newNode kb (newNode ka l c) r

-- | AVL Tree Left Right rotation
rotateLR :: (ByteArrayAccess key, ByteArrayAccess value)
         => (AVLTree key value) -> (AVLTree key value)
rotateLR (Node _ _ ka _ l (Node _ _ kb _ (Node _ _ kc _ m n) r))
       = newNode kc (newNode ka l m) (newNode kb n r)

-- | AVL Tree Right rotation
rotateR :: (ByteArrayAccess key, ByteArrayAccess value)
        => (AVLTree key value) -> (AVLTree key value)
rotateR (Node _ _ ka _ (Node _ _ kb _ l c) r)
      = newNode kb l (newNode ka c r)

-- | AVL Tree Right Left rotation
rotateRL :: (ByteArrayAccess key, ByteArrayAccess value)
         => (AVLTree key value) -> (AVLTree key value)
rotateRL (Node _ _ ka _ (Node _ _ kb _ l (Node _ _ kc _ m n)) r)
       = newNode kc (newNode kb l m) (newNode ka n r)

-- | compares heights of kids of a given node
compareKidsHeight :: (AVLTree key value) -> Ordering
compareKidsHeight (Node h _ ka la l r) = compare (height l) (height r)
compareKidsHeight _ = EQ

-- | AVL balancing operation
balance :: (ByteArrayAccess key, ByteArrayAccess value)
        => (AVLTree key value) -> (AVLTree key value)
balance (Node h _ ka la l r) =
  case (height r) - (height l) of
    2 -> case compareKidsHeight r of
           GT -> rotateLR (Node h True ka la l r)
           _  -> rotateL  (Node h True ka la l r)
    -2 -> case compareKidsHeight l of
           LT -> rotateRL (Node h True ka la l r)
           _  -> rotateR  (Node h True ka la l r)
    _ -> (Node h True ka la l r)
balance leaf = leaf

-- | helps with modifying
modifyMin :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
          => (Operation key value)            -- ^ operation, applied to old tree
          -> (AVLTree key value)              -- ^ old tree
          -> (Maybe value, AVLTree key value) -- ^ (return value, new tree)
modifyMin op MinLeaf = modify op MinLeaf
modifyMin op (Leaf k v) = modify op (Leaf k v)
modifyMin op (Node _ _ k _ l r) = (mv, balance (newNode k mp r))
  where
    (mv, mp) = modifyMin op l

-- | helps with deletion
replaceLeftLeaf :: (ByteArrayAccess key, ByteArrayAccess value)
                => (AVLTree key value)              -- ^ inserting instead of leaf
                -> (AVLTree key value)              -- ^ tree where new leaf is inserted
                -> (Maybe value, AVLTree key value) -- ^ (deleted value, resulting tree)
replaceLeftLeaf leaf (Node _ _ k _ l r) = (v, newNode k left r)
  where (v, left) = (replaceLeftLeaf leaf l)
replaceLeftLeaf leaf (Leaf _ v)         = (Just v, leaf)
replaceLeftLeaf leaf MinLeaf            = (Nothing, leaf)

-- | helps with deletion
deleteRight :: (ByteArrayAccess key, ByteArrayAccess value)
            => (AVLTree key value)    -- ^ initial tree
            -> ( key                  -- ^ key of deleted leaf
               , (AVLTree key value)  -- ^ deleted leaf
               , (AVLTree key value)) -- ^ new tree
deleteRight (Node _ _ k _ l r) =
  case r of
    (Leaf rKey rVal) -> (rKey, (Leaf rKey rVal), l)
    _                -> (delKey, delLeaf, balance (newNode k l delRight))
  where
    (delKey, delLeaf, delRight) = deleteRight r

-- | helps to collect answers in modifyMany
ansCollector :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
             => ([Maybe value], AVLTree key value)
             -> (Operation key value)
             -> ([Maybe value], AVLTree key value)
ansCollector (res, t) op = ((res ++ [fst p]), snd p)
  where p = modify op t



