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

generateChain' :: Map String [String] -> Int -> String -> String -> String
generateChain' _ 0 _ r = r
generateChain' m n s r = generateChain' m (n-1) (chooseWord) (r ++ " " ++ s)
  where chooseWord = (m ! s) !! 0
