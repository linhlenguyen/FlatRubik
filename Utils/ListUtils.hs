module Utils.ListUtils(
replace,
zipL,
reOrderList,
containSameElems,
filterDuplicates
)
where
  replace :: [Int] -> [t] -> [t] -> [t]
  replace xs rs ys = replace' 0 xs rs ys

  replace' :: Int -> [Int] -> [t] -> [t] -> [t]
  replace' i (x:xs) (r:rs) (y:ys) = if (i == x) then r : replace' (i+1) xs rs ys
                                                else y : replace' (i+1) (x:xs) (r:rs) ys

  zipL :: [[t]] -> [t] -> [[t]]
  zipL _ [] = []
  zipL [] t = map (\x -> [x]) t
  zipL (x:xs) (y:ys) = (y:x) : zipL xs ys

  --Re order list from starting from element t and loop around
  reOrderList :: (Eq t) => t -> [t] -> [t]
  reOrderList x (y:ys) = if y == x then x : ys else reOrderList x (ys ++ [y])

  containSameElems :: (Eq t) => [t] -> [t] -> Bool
  containSameElems [] [] = True
  containSameElems _ [] = False
  containSameElems [] _ = False
  containSameElems (x:xs) ys = if any (\k -> k == x) ys then containSameElems xs (filter (\k -> k /= x) ys) else False

  filterDuplicates :: (Eq t) => [[t]] -> [[t]]
  filterDuplicates [] = []
  filterDuplicates (x:xs) = x : filterDuplicates newList
    where newList = (filter (\k -> not $ containSameElems x k) xs)
