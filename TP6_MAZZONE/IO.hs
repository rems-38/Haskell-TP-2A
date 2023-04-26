import Data.List
import System.Environment

afficher_avec_etoiles :: Show a => a -> IO ()
afficher_avec_etoiles x = print ("***" ++ show x ++ "***")

echo :: IO ()
echo = getLine >>= putStrLn

wc :: String -> IO ()
wc file =
    do
        contenu <- readFile file
        let nlines = length (lines contenu)
            nwords = length (words contenu)
            nchar = length contenu -- 1 char => 1 octet
        putStrLn $ show nlines ++ " " ++ show nwords ++ " " ++ show nchar ++ " " ++ file

main :: IO ()
main =
    do
        args <- getArgs
        case args of
            [] -> echo
            [file] -> wc file