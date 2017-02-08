module Data.Square(
Square(..),
newSquare
)
  where
    import qualified Graphics.Gloss as Gloss

    data Square = Square {
      color :: Gloss.Color,
      size :: Float
    }
    deriving (Show)

    newSquare :: Gloss.Color -> Square
    newSquare c = Square {
      color = c,
      size = 1.0f
    }
