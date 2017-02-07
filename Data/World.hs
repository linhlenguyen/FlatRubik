module Data.World(
World(..),
newWorld
)
where
  import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
  import Data.Rubik
  import Data.Node

  data World = World {
    rubik :: Rubik,
    keyPressed :: [Gloss.Key],
    dt :: Float,
    selectedSide :: Int,
    selectedNode :: Maybe Int
  }

  newWorld :: World
  newWorld = World {
    rubik = newRubik 3,
    keyPressed = [],
    dt = 0,
    selectedSide = 1,
    selectedNode = Nothing
  }
