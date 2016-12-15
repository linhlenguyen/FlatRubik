module Data.Node(
Node(..),
newNode
)
  where
    import qualified Graphics.Gloss as Gloss

    data Node = Node {
      color :: Gloss.Color
    }

    newNode :: Color -> Node
    newNode c = Node {
      color = c
    }
