
toPairs :: [a] -> [(a, [a])] -> [(a, [a])]
toPairs (x:y:ys) acc = toPairs (y : ys) $ (x, y : []) : acc
toPairs _ acc = acc

