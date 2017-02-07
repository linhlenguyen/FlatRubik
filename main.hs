module Main(main)
where
  import Graphics.Gloss.Interface.Pure.Game
  import AppSetup
  import Data.World
  import Data.Rubik
  import Renderer.Renderer
  import Control.Control
  
  update :: Float -> World -> World
  update _ r = r

  main :: IO ()
  main = play window background fps newWorld renderRubik handleKeyPress update
