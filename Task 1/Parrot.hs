import System.IO
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Typeable (typeOf)
import System.Random
import Data.Time.Clock
import Data.Time.LocalTime

readFileToList :: [FilePath] -> IO [String]
readFileToList filePaths = fmap concat (mapM readFileToWords filePaths)

readFileToWords :: FilePath -> IO [String]
readFileToWords filePath = do
    contents <- readFile filePath
    --print (take 100 (words contents))
    return (map (map toLower) (words contents))



-- bigram dictionary: save all the words with the next word and the count of the next word
bigramdict cont = foldr updateModel Map.empty (pairs cont)
  where
    pairs :: [a] -> [(a, a)]
    pairs [] = []
    pairs [_] = []
    pairs (x:y:conttail) = (x, y) : pairs (y:conttail)

    updateModel :: (String, String) -> Map String [(String, Int)] -> Map String [(String, Int)]
    updateModel (x, y) bigramMap = case Map.lookup x bigramMap of
        Just ys -> Map.insert x (updateWords y ys) bigramMap
        Nothing -> Map.insert x [(y, 1)] bigramMap

    updateWords :: String -> [(String, Int)] -> [(String, Int)]
    updateWords word [] = [(word, 1)]
    updateWords word ((w, count) : ws)
        | w == word = (w, count + 1) : ws
        | otherwise = (w, count) : updateWords word ws

-- bigram dictionary: save all the words with the next word following that; do not save the count of the next word
bigramdict2 cont = foldr updateModel Map.empty (pairs cont)
  where
    pairs :: [a] -> [(a, a)]
    pairs [] = []
    pairs [_] = []
    pairs (x:y:conttail) = (x, y) : pairs (y:conttail)

    updateModel :: (String, String) -> Map String [String] -> Map String [String]
    updateModel (x, y) bigramMap = case Map.lookup x bigramMap of
        Just ys -> Map.insert x (updateWords y ys) bigramMap
        Nothing -> Map.insert x [y] bigramMap

    updateWords :: String -> [String] -> [String]
    updateWords word [] = [word]
    updateWords word (w : ws)
        | w == word = w : ws
        | otherwise = w : updateWords word ws

-- trigram dictionary: save all the words with the next two words and the count of the next two words
trigramdict cont = foldr updateModel Map.empty (triples cont)
  where
    triples :: [a] -> [(a, a, a)]
    triples [] = []
    triples [_] = []
    triples [_, _] = []
    triples (x:y:z:conttail) = (x, y, z) : triples (y:z:conttail)

    updateModel (x, y, z) trigramMap = case Map.lookup x trigramMap of
        Just ys -> Map.insert x (updateWords y z ys) trigramMap
        Nothing -> Map.insert x [(y, z, 1)] trigramMap

    updateWords word1 word2 [] = [(word1, word2, 1)]
    updateWords word1 word2 ((w1, w2, count) : ws)
        | w1 == word1 && w2 == word2 = (w1, w2, count + 1) : ws
        | otherwise = (w1, w2, count) : updateWords word1 word2 ws

-- trigram dictionary 2: save all the words with the next two words following that; do not save the count of the next two words
trigramdict2 cont = foldr updateModel Map.empty (triples cont)
  where
    triples :: [a] -> [(a, a, a)]
    triples [] = []
    triples [_] = []
    triples [_, _] = []
    triples (x:y:z:conttail) = (x, y, z) : triples (y:z:conttail)

    updateModel (x, y, z) trigramMap = case Map.lookup x trigramMap of
        Just ys -> Map.insert x (updateWords y z ys) trigramMap
        Nothing -> Map.insert x [(y, z)] trigramMap

    updateWords word1 word2 [] = [(word1, word2)]
    updateWords word1 word2 ((w1, w2) : ws)
        | w1 == word1 && w2 == word2 = (w1, w2) : ws
        | otherwise = (w1, w2) : updateWords word1 word2 ws

-- trigram dictionary: save each two words as key and the next word as value
trigramdict3 cont = foldr updateModel Map.empty (triples cont)
  where
    triples :: [a] -> [(a, a, a)]
    triples [] = []
    triples [_] = []
    triples [_, _] = []
    triples (x:y:z:conttail) = (x, y, z) : triples (y:z:conttail)

    updateModel (x, y, z) trigramMap = case Map.lookup (x, y) trigramMap of
        Just ys -> Map.insert (x, y) (updateWords z ys) trigramMap
        Nothing -> Map.insert (x, y) [z] trigramMap

    updateWords word [] = [word]
    updateWords word (w : ws)
        | w == word = w : ws
        | otherwise = w : updateWords word ws


getContent = do  
    let book1 = "Text Files/A_Book_of_Discovery.txt"
        book2 = "Text Files/A_Literary_and_Historical_Atlas_of_Asia.txt"
        book3 = "Text Files/Celebrated_Travels.txt"
        book4 = "Text Files/Commercial_Geography.txt"
        book5 = "Text Files/Darwin_and_Modern_Science_by_A_C_Seward.txt"
        book6 = "Text Files/Days_of_the_Discoverers.txt"
        book7 = "Text Files/Outlines_of_Dairy_Bacteriology.txt"
        book8 = "Text Files/Seaside_Studies_in_Natural_History.txt"
        book9 = "Text Files/The_Beauties_of_Nature.txt"
        book10 = "Text Files/The_Earth_and_its_inhabitants_Volume1_Europe.txt"
        
        -- filePaths = [book1, book2, book3, book4, book5, book6, book7, book8, book9, book10]

        test = "Text Files/test.txt"
        filePaths = [test]

        -- filePaths = [book1]
        
    bookContent <- readFileToList filePaths
    return bookContent
    -- putStrLn "Type of the first word:"
    -- print (typeOf (head bookContent))
    -- mapM_ putStrLn (take 100 bookContent)


preprocess [] = []
preprocess (x:xs) = (map toLower (filter (\c -> isAlphaNum c || isSpace c) x)) : preprocess xs

-- 

main = do
    bookContent <- getContent
    let processesedContent = preprocess bookContent

    let unigram = processesedContent
    let bigram = bigramdict2 processesedContent
    let trigram = trigramdict3 processesedContent

    print "Unigram:"
    print unigram
    print "Bigram:" 
    print bigram
    print "Trigram:"
    print trigram