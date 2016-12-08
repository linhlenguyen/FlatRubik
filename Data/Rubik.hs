module Rubik(

)
where
  import Data.Node
  --loopable list?

  upN :: Int -> Int -> Int
  upN dimension index = if index >= dimension then index - dimension
                         else dimension*(dimension-1) + index

  leftN :: Int -> Int -> Int
  leftN dimension index = if (mod index dimension) == 0 then index - (dimension + 1)
                          else index + 1

  rightN :: Int -> Int -> Int
  rightN dimension index = if (mod index dimension) == 1 then index + (dimension - 1)
                           else index - 1

  downN :: Int -> Int -> Int
  downN dimension index = if (index < dimension*(dimension -1)) then index - dimension
                            else mod index dimension

  --top, left, right, down
  sideMap :: [(Char, [Char])]
  sideMap = [('A',['F','B','D','C']),
    ('B',['A','D','C','E']),
    ('C',['A','B','D','E']),
    ('D',['A','C','F','E']),
    ('E',['C','B','D','F']),
    ('F',['E','B','D','A'])]
