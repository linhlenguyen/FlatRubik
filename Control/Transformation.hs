module Control.Transformation(
Direction(..),
rotate
)
where
  import qualified Data.Array as Array
  import qualified Data.Map as Map
  import Data.Rubik
  import Utils.ListUtils

  --Notes
  --Another potential solutions to rotation transformation
  --Given a 3x3 Rubik cube, there are only 9 possible moves that can be made
  --For nxn cube, there are 3n possible moves
  --The cube can be modelled in an array and the change of state can be hard coded
  --This will also make searching for solution easier,
  --However it'll be more difficult to scale the dimension of the cube

  --Given a Rubik

  --    [6]
  --    [4]
  --[5] [1] [3]
  --    [2]

  --4 Direction of rotation are Up, Down, Left, Right
  --3 Traversal path are
  -- 1 - 3 - 6 - 5 (p1)
  -- 1 - 2 - 6 - 4 (p2)
  -- 2 - 3 - 4 - 5 (p3)

  --On each side there are 2 possible paths of traversal
  --1 -> p1 & p2
  --3 -> p2 & p3

  --On each side selected node have *row* and *column* indexes

  --If the Rubik is stored in the following format.
  --(1, [R, R, R, R, R, R, R, R])
  --(2, [G, G, G, G, G, G, G, G])
  --(3, [B, B, B, B, B, B, B, B])
  --(4, [Y, Y, Y, Y, Y, Y, Y, Y])
  --(5, [O, O, O, O, O, O, O, O])
  --(6, [W, W, W, W, W, W, W, W])

  --If a 6 sided Rubik is model as 4x4 set of sides as followed
  -- [3] [6] [5] [1]
  -- [] [4] [] [2]
  -- [5] [1] [3] [6]
  -- [] [2] [] [4]

  --

  data Direction = Clockwise | CounterClockwise | Up | Down | Left | Right deriving (Show, Eq)

  rotate :: Direction -> Int -> Int -> Int -> Rubik -> Rubik
  rotate direction dimension side node r = r'
    where row = div node dimension
          column = mod node dimension
          r' = r

  rotateVertical :: Direction -> Int -> Int -> Rubik -> Rubik
  rotateVertical direction side column r = r'
    where traversalList = reOrderList side $ (sideTraversalMap!side)!Vertical
          rotationSides = []
          shiftList = if Direction == Up then traversalList else reverse traversalList
          [side1,side2,side3,side4] = map (r!) rotationList
          side1' = swapColumn column (getColumn column side4) side1
          side2' = swapColumn column (getColumn column side1) side2
          side3' = swapColumn column (getColumn column side2) side3
          side4' = swapColumn column (getColumn column side3) side4
          newM = zip shiftList [side1', side2', side3', side4']
          r' = Map.union r newM

  rotateHorizontal :: Direction -> Int -> Int -> Rubik -> Rubik
  rotateHorizontal direction side row r = r'
    where traversalList = reOrderList side $ (sideTraversalMap!side)!Horizontal
          rotationList = if Direction == Control.Transformation.Right then traversalList else reverse traversalList
          [side1,side2,side3,side4] = map (r!) rotationList
          side1' = swapRow row (getRow row side4) side1
          side2' = swapRow row (getRow row side1) side2
          side3' = swapRow row (getRow row side2) side3
          side4' = swapRow row (getRow row side3) side4
          r' = Map.fromList $ zip rotationList [side1',side2',side3',side4']

  rotateSide :: Direction -> [[t]] -> [[t]]
  rotateSide Clockwise r = map reverse $ foldl zipL [] r
  rotateSide CounterClockwise r = reverse $ foldl zipL [] r

  swapRow :: Int -> [t] -> [[t]] -> [[t]]
  swapRow row r rs = replace 0 row r rs

  swapColumn :: Int -> [t] -> [[t]] -> [[t]]
  swapColumn column xs r = rotateSide CounterClockwise $ replace 0 column r $ rotateFace Clockwise r

  getRow :: Int -> [[t]] -> [t]
  getRow row r = r!!row

  getColumn :: Int -> [[t]] -> [t]
  getColumn column r = (rotateSide Clockwise r)!!column

  data Traversal = Vertical | Horizontal

  -- Lists of all side traversal possibilities
  sideTraversals :: Array.Array Int [Int]
  sideTraversals = array (1,3) [(1,[1,2,6,4]),(2,[1,3,6,5]),(3,[2,3,4,5])]

  sideTraversalMap :: Map.Map Int Map.Map Traversal [Int]
  sideTraversalMap = Map.fromList [(1, Map.fromList [(Vertical, sideTraversals!1), (Horizontal, sideTraversals!2)]),
    (2, Map.fromList [(Vertical, sideTraversals!1), (Horizontal, sideTraversals!3)]),
    (3, Map.fromList [(Vertical, sideTraversals!2), (Horizontal, sideTraversals!3)]),
    (4, Map.fromList [(Vertical, sideTraversals!1), (Horizontal, sideTraversals!3)]),
    (5, Map.fromList [(Vertical, sideTraversals!2), (Horizontal, sideTraversals!3)]),
    (6, Map.fromList [(Vertical, sideTraversals!1), (Horizontal, sideTraversals!2)])]
