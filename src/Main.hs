module Main where
import Data.Char (isDigit)
import GHC.Unicode (isSpace)

data Token =
    TNumber Int |
    TString String |
    TBraceOpen |
    TBraceClose |
    TParanOpen |
    TParanClose
    deriving (Show)

main :: IO ()
main = do
    contents <- readFile "test.txt"
    putStrLn contents
    case tokenize contents of
        Left err -> putStrLn ("Error: " ++ err)
        Right tokens -> mapM_ print tokens
    writeFile "output.txt" contents

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (c:cs)
    | isSpace c = tokenize cs
    | isDigit c = do
        let (digits, rest) = span isDigit (c:cs)
        let num = read digits
        rest_tokens <- tokenize rest
        return (TNumber num : rest_tokens)
    | c == '"' = do
        let (str, rest) = span  (/= '"') cs
        let rest_tok = drop 1 rest
        rest_tokens <- tokenize rest_tok
        return (TString str : rest_tokens)
    | c == '{' = do
        rest <- tokenize cs
        return (TBraceOpen : rest)
    | c == '}' = do
        rest <- tokenize cs
        return (TBraceClose : rest)
    | c == '(' = do
        rest <- tokenize cs
        return (TParanOpen : rest)
    | c == ')' = do
        rest <- tokenize cs
        return (TParanClose : rest)
    | otherwise = Left ("Unknown character |" ++ [c] ++ "|")
