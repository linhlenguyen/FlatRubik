module Data.Node(
Colour(..)
Node(..)
)
  where
    data Colour = Red | Green | Blue | Organe | White | Yellow

    data Node = Node {
      colour :: Colour,
      up :: Node,
      left :: Node,
      right :: Node,
      down :: Node
    }
