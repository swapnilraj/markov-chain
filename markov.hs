import Data.Map as Map
import System.Random

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

generateChain' :: Map String [String] -> Int -> IO String -> IO String -> IO String
generateChain' _ 0 _ r = r
generateChain' m n s r =
    r >>= \r' ->
    s >>= \s' ->
    generateChain' m (n-1) (chooseRandomWord) $ pure (r' ++ " " ++ s')
  where
    wordList = s >>= pure . (m !)
    chooseRandomWord = wordList >>= \wordList' ->
      randomRIO (0, length wordList' - 1) >>= pure . (wordList' !!)

generateChain :: Map String [String] -> Int -> String -> IO String
generateChain m n s = generateChain' m n (pure s) (pure "")

main =
  putStrLn "Hey what word should the speech start with?" >>
    getLine >>= \word -> putStrLn "How many words do you want it to be?" >>
      getLine >>= \len -> readCorpus >>=
        \map -> generateChain map (read len :: Int) word
