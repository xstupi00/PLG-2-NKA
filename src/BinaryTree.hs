module BinaryTree where

-- ^ Define data structure for the binary search tree.
-- ^ The tree items contain key and data.
-- ^ Write the function INSERT, which add item to the tree.
-- ^ When tree contains the item with the same key, then the data will be actualized.
-- ^ Bonus point for the correct type of the function INSERT.

data Tree key data'
  = Nil
  | Node key data' (Tree key data') (Tree key data')

insert :: Ord key => key -> data' -> Tree key data' -> Tree key data'
-- ^ empty tree, then insert root with empty left and right nodes
insert key data' Nil = Node key data' Nil Nil
-- ^ non-empty tree, matching current node and its sub-trees
insert key data' (Node current_key current_data leftTree rightTree)
  -- ^ keys are equals -> update data
  | key == current_key = Node key data' leftTree rightTree
  -- ^ going to the left subtree of the current node
  | key < current_key  = Node current_key current_data (insert key data' leftTree) rightTree
  -- ^ going to the right subtree of the current node
  | key >= current_key = Node current_key current_data leftTree (insert key data' rightTree)
  

-- ^ Define data structure for the binary search tree.
-- ^ The tree items contain key and data.
-- ^ Write the function FIND, which try find item according to the given key
-- ^ Catch the situation where the tree do not contain searching key

find :: Ord key => key -> Tree key data' -> Maybe data'
-- ^ key was not found (empty tree) - tree do not contain searching tree 
find _ Nil = Nothing
-- ^ matching the root node and its sub-trees
find key (Node current_key current_data leftTree rightTree)
  -- ^ key are equals, then returns data
  | key == current_key = Just current_data
  -- ^ continue in the left sub-trees 
  | key < current_key = find key leftTree
  -- ^ continue in the right sub-trees
  | key >= current_key = find key rightTree