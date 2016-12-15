module Rubik(
newRubik
)
where
  import qualified Data.Map as Map
  import qualified Data.Array as Array
  import qualified Graphics.Gloss as Gloss
  import Data.Node
  --A loopable list?

  data Direction = Clockwise | CounterClockwise | Up | Down | Left | Right deriving (Show)

  colors :: [Gloss.Color]
  colors = [Gloss.red, Gloss.green, Gloss.blue, Gloss.yellow, Gloss.rose, Gloss.violet]

  type Rubik = Map.Map Int Array.Array Int Gloss.Color

  newRubik :: Int -> Rubik
  newRubik dimension = Map.fromList $ zip [1..6] $ map toArray colors
    where arraySize = dimension^2
          toArray :: Gloss.Color -> Array Int Gloss.Color
          toArray c = Array.array (1,arraySize) $ zip [1..arraySize] $ replicate arraySize c

  rotate :: Direction -> Int -> Int -> Int -> Rubik -> Rubik
  rotate direction dimension side node r = r'
    where row = div node dimension
          column = mod node dimension
          r' = r

  rotateFace :: Direction -> Array Int t -> Array Int t
  rotateFace Clockwise ar = undefined
  rotateFace CounterClockwise ar = undefined

  swapRow :: Int -> Int -> [t] -> Array Int t -> Array Int t
  swapRow dimension row xs ar = undefined

  swapColumn :: Int -> Int -> [t] -> Array Int t -> Array Int t
  swapColumn dimension column xs ar = undefined

  getRow :: Int -> Int -> Array Int t -> [t]
  getRow dimension row ar = undefined

  getColumn :: Int -> Int -> Array Int t -> [t]
  getColumn dimension column ar = undefined
