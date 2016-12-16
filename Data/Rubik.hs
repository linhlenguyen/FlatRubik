module Rubik(
newRubik
)
where
  import qualified Data.Map as Map
  import qualified Graphics.Gloss as Gloss
  import Data.Node
  --A loopable list?

  colors :: [Gloss.Color]
  colors = [Gloss.red, Gloss.green, Gloss.blue, Gloss.yellow, Gloss.rose, Gloss.violet]

  type Rubik = Map.Map Int [[Gloss.Color]]

  newRubik :: Int -> Rubik
  newRubik dimension = Map.fromList $ zip [1..6] $ map toArray colors
    where arraySize = dimension^2
          toArray :: Gloss.Color -> [Gloss.Color]
          toArray c = replicate arraySize c
