import System.IO
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Typeable (typeOf)


-- fmap: https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Functor.html

readFileToList :: [FilePath] -> IO [String]
readFileToList filePaths = fmap concat (mapM readFileToWords filePaths)

readFileToWords :: FilePath -> IO [String]
readFileToWords filePath = do
    contents <- readFile filePath
    print (take 100 (words contents))
    return (words contents)
  

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
        
        filePaths = [book1, book2, book3, book4, book5, book6, book7, book8, book9, book10]

    bookContent <- readFileToList filePaths
    return bookContent



    -- putStrLn "Type of the first word:"
    -- print (typeOf (head bookContent))
    -- mapM_ putStrLn (take 100 bookContent)

main = do
    bookContent <- getContent
    print (take 100 bookContent)
