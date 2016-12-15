module World(
newWorld
)
where
  import
  import Data.Rubik

  data World = World {
    rubik :: [[Node]],
    keyPressed :: [Key],
    dt :: Float,
    selectedSide :: Int,
    selectedNode :: Maybe Int
  }

  newWorld :: World
  newWorld = World {
    rubik = newRubik,
    keyPressed = [],
    dt = 0,
    selectedSide = 1,
    selectedNode = Nothing
  }
