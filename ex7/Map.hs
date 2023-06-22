module Main
where
import Prelude hiding (lookup)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

-- The Map interface and implementation
type Map key value = key -> Maybe value

empty :: Map key value
empty _ = Nothing

insert :: Eq key => (key, value) -> Map key value -> Map key value
insert (k, v) m x = if k == x then Just v else m x

lookup :: key -> Map key value -> Maybe value
lookup x m = m x

fromList :: Eq key => [(key, value)] -> Map key value
fromList = foldr insert empty


-- a)
queryKey :: IO String
queryKey = do {
    putStr "Enter the key: ";
    getLine
}

-- b)
queryValue :: IO Integer
queryValue = do
    putStr "Enter the value: "
    input <- getLine

    case reads input of
        [(value, "")] -> return value
        _ -> do
            putStrLn "invalid input (must be an integer)"
            queryValue


-- c)

data Action = Insert | Lookup | Exit

queryAction :: IO Action
queryAction = do
    putStr "Do you want to insert (i), lookup (l) or exit (e)? [i/l/e] "
    input <- getLine
    case input of
        "i" -> return Insert
        "l" -> return Lookup
        "e" -> return Exit
        _ -> do
            putStrLn "invalid input (must be either i, l, or e)"
            queryAction

-- d)

loop :: Map String Integer -> IO ()
loop m = do
    action <- queryAction
    case action of
        Insert -> do
            key <- queryKey
            value <- queryValue
            loop $ insert (key, value) m
        Lookup -> do
            key <- queryKey
            print $ lookup key m
            loop m
        Exit -> do
            return ()


-- e)
-- We just start the loop with an empty map.
-- The "hSetBuffering stdout NoBuffering" part is to disable output buffering,
-- otherwise output written by putStr might not become visible when executing the compiled program.

main :: IO ()
main = hSetBuffering stdout NoBuffering >> loop empty
