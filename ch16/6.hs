{-
6.  Suppose the following type is given.
      data Tree = Leaf Int | Node Tree Tree
    Prove by induction that the number of leaves on this Tree is always one more than
    the number of nodes.

    Proof:
    First, we define the following functions that count the number of leaves
    and nodes of a tree, respectively.
    
      leaves :: Tree -> Int
      leaves (Leaf _)   = 1
      leaves (Node l r) = leaves l + leaves r
    
      nodes :: Tree -> Int
      nodes (Leaf _)    = 0
      nodes (Node l r)  = 1 + nodes l + nodes r 

    Here, it is sufficient to show that 
      nodes a + 1 = leaves a
    for any a::Tree.

    We prove this by induction on a
    --- case: a = Leaf n
      nodes a + 1 = 1
      leaves a    = 1
    
    -- case: a = Node l r
      nodes (Node l r) 
    = 1 + nodes l + nodes r
    
      leaves (Node l r)
    = leaves l + leaves r
    = 1 + nodes l + nodes r + 1
    QED:
-}