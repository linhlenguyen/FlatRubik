module Renderer.Renderer(
renderRubik
)
where
  import qualified Graphics.Gloss as Gloss
  import Data.World
  import Data.Rubik

  squareSize :: Float
  squareSize = 10.0

  faceSize :: Float
  faceSize = undefined

  renderRubik :: World -> Gloss.Picture
  renderRubik = undefined
