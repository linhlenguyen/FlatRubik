module Control.Control(
handleEvent
)
where
  import Graphics.Gloss.Interface.Pure.Game
  import Data.World
  import Data.Rubik

  -- Note on controls

  -- Keyboard control
  -- A square cursor to select which side of the rubik is currently active (6 side in total at the moment)
  -- Cursor can be moved with arrow keys
  -- Numlock keys (from 1 -> 9) can be used to select the square to turn
  -- A S D W can be used to rotate that square in 4 directions

  -- Mouse Control
  -- Need to map potision on screen to correct square represented in Rubik data structure
  -- Click and drag mouse to rotate the cube

  handleEvent :: Float -> Key -> World -> World
  handleEvent t key ws = ws

  handleKeyPress :: Event -> World -> World
  handleKeyPress (EventKey key Up _ _) ws = ws'
    where
      ws' = resetKey key ws
  handleKeyPress (EventKey key Down _ _) ws = ws'
    where
      keys = keyPressed ws
      ws' = if any (\k -> k == key) keys then ws else ws { keyPressed = key:keys }
  handleKeyPress _ ws = ws

  resetKey :: Key -> World -> World
  resetKey key ws = ws'
    where keys = keyPressed ws
          ws' = ws { keyPressed = filter (\k -> not (k == key)) keys }

  keyEvent :: Float -> World -> World
  keyEvent t ws = ws'
    where keys = keyPressed ws
          ct = dt ws
          ws' = if null keys then ws
                else foldr (handleEvent t) ws keys
