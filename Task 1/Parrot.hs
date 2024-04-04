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


preprocess [] = []
preprocess (x:xs) = (map toLower (filter (\c -> isAlphaNum c || isSpace c) x)) : preprocess xs

-- create random sentence based on unigram dictionary
-- gen random number between 0 and length of unigram; get words at that index and create sentence
createRandomSentenceUnigram unigram n = do
    gen <- getStdGen
    let randomWords = take n (randomRs (0, (length unigram) - 1) gen)
    let sentence = unwords (map (\i -> unigram !! i) randomWords)
    print sentence

-- create random sentence based on bigram dictionary
-- get random first word and then get the next word based on the first word; repeat n times
createRandomSentenceBigram bigram n = do
    gen <- getStdGen
    let (firstWord_index, newgen) = randomR (0, length (Map.keys bigram) - 1) gen
    let firstWord = (Map.keys bigram) !! firstWord_index
    let sentence = unwords (take n (firstWord : createRandomSentenceBigramHelper bigram (n-1) firstWord (newgen :: StdGen)))
    print sentence

-- create random sentence based on bigram dictionary
-- based on word, get random indices and get the next word at that index; repeat n times
-- if word is empty, get random word
createRandomSentenceBigramHelper bigram n word gen
    | n == 0 = []
    | word == "" =  let (word_index2, newGen) = randomR (0, length (Map.keys bigram) - 1) gen
                        word2 = (Map.keys bigram) !! word_index2
                            in word2 : createRandomSentenceBigramHelper bigram (n-1) word2 newGen
    | otherwise = nextWord : createRandomSentenceBigramHelper bigram (n-1) nextWord newGen
    where
        number_of_words = length (bigram Map.! word) -1
        (nextWord_index, newGen) = randomR (0, number_of_words) gen
        nextWord = getWordFromBigramList word nextWord_index bigram

-- from bigram map based on word given - get the followed word at position i; return "" if word not found
getWordFromBigramList word i bigram = case Map.lookup word bigram of
    Just ys -> ys !! i
    Nothing -> ""

-- main function: get content from files, preprocess the content, create unigram, bigram and trigram dictionaries, generate random sentences
main = do
    bookContent <- getContent
    let processesedContent = preprocess bookContent

    let unigram = processesedContent
    let bigram = bigramdict2 processesedContent
    let trigram = trigramdict3 processesedContent

    --print "Unigram:"
    --print unigram
    putStrLn "Bigram:" 
    print bigram
    --print "Trigram:"
    --print trigram

    putStrLn "Unigram sentence:" 
    createRandomSentenceUnigram unigram 20
    putStrLn "Bigram sentence: "
    createRandomSentenceBigram bigram 20
    -- print(getWordFromBigramList "Alabama" 0 bigram)

    