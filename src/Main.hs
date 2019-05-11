module Main where

import Control.Monad.Writer
import Data.DList hiding (head)
import Data.Map as Map
import System.Random

readCorpus :: IO (Map String [String])
readCorpus = toMap . flip toPairs mempty . words <$> readFile "speeches.txt"

combineCorpus :: [a] -> [a] -> [a]
combineCorpus a b = head a : b

toMap :: Ord a => [(a, [a])] -> Map a [a]
toMap = Map.fromListWith combineCorpus

toPairs :: [a] -> [(a, [a])] -> [(a, [a])]
toPairs (x:y:ys) acc = toPairs (y : ys) $ (x, [y]) : acc
toPairs _ acc = acc

generateChain :: Map String [String] -> Int -> String -> WriterT (DList String) IO ()
generateChain _ 0 s = tell $ pure s
generateChain m n s =
  do
    tell $ pure s
    randomWord <- lift chooseRandomWord
    generateChain m (pred n) randomWord
  where
    wordList = m ! s
    chooseRandomWord = (wordList !!) <$> randomRIO (0, pred $ length wordList)

main = do
  map <- readCorpus
  print "Hey what word?"
  word <- getLine
  print "How many words?"
  len <- getLine
  sentence <- execWriterT $ generateChain map (read len) word
  print sentence
