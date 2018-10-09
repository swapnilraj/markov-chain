import Data.Map as Map

readCorpus :: IO (Map String [String])
readCorpus =
  readFile "speeches.txt" >>= pure . (flip toPairs []) . words

toPairs :: [a] -> [(a, [a])] -> [(a, [a])]
toPairs (x:y:ys) acc = toPairs (y : ys) $ (x, y : []) : acc
toPairs _ acc = acc

