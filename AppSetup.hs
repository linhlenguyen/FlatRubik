module AppSetup(
window,
background,
fps,
initialState
)
where
  import qualified Graphics.Gloss as Gloss
  import Data.Rubik

  window :: Gloss.Display
  window = Gloss.InWindow "Flat Rubik" screen (10,10)

  background :: Gloss.Color
  background = Gloss.white

  initialState :: Rubik
  initialState = newRubik 3

  fps = 30::Int

  screen :: (Int, Int)
  screen = (800, 800)
