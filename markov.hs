import Data.Map as Map

readCorpus :: IO (Map String [String])
readCorpus =
  readFile "speeches.txt" >>= pure . toMap . (flip toPairs []) . words

combineCorpus :: [a] -> [a] -> [a]
combineCorpus a b = head a : b

toMap :: Ord a => [(a, [a])] -> Map a [a]
toMap = Map.fromListWith combineCorpus

toPairs :: [a] -> [(a, [a])] -> [(a, [a])]
toPairs (x:y:ys) acc = toPairs (y : ys) $ (x, y : []) : acc
toPairs _ acc = acc
