
module Main
where
import System.Environment

countNewLines :: String -> Int
countNewLines = length . lines

countWords :: String -> Int
countWords = length . words

countBytes :: String -> Int
countBytes = length

getData :: FilePath -> IO (Int, Int, Int)
getData filePath = do
    f <- readFile filePath
    return (countNewLines f, countWords f, countBytes f) 

displayFile :: FilePath -> IO ()
displayFile filePath = do
    file <- readFile filePath
    putStrLn (show (countNewLines file) ++ " " ++ show (countWords file) ++ " " ++ show (countBytes file) ++ " " ++ filePath)

tupleToList :: (a, a, a) -> [a]
tupleToList (x, y, z) = [x, y, z]

displayTotal :: Show a => [a] -> IO ()
displayTotal [] = putStrLn ""
displayTotal (x:xs) = do 
    putStr (show x ++ " ")
    displayTotal xs

main :: IO ()
main = do
    args <- getArgs
    datas <- mapM getData args
    mapM_ displayFile args
    displayTotal $ map sum $ tupleToList $ unzip3 datas
