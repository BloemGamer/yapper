module Main where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Data.Map as Map

type MacroTable = Map.Map String String

data Definition
  = DKeyword
  | DIgnore
  | DToken
  | DLiteral
  | DStartWith
  | Comment
  | Macro
  | StartWith
  deriving (Show)

data Modifier = MCallFunc | MType deriving (Show)

data TokenType
  = TNumber Int
  | TString String
  | TDefinition Definition
  | TIdent String
  | TChar String
  | TModifier Modifier
  | TMacro String
  | TBraceOpen
  | TBraceClose
  | TParanOpen
  | TParanClose
  | TColon
  | TSemicolon
  | TComma
  | TDot
  | TDotDot
  | TStar
  | TPlus
  | TMinus
  | TArrow
  | TSlash
  | TEOF
  deriving (Show)

data Token = Token {token_type :: TokenType, token_span :: (Int, Int)} deriving (Show)

main :: IO ()
main = do
  contents <- readFile "input/lex.yapper"
  putStrLn contents
  case tokenize contents of
    Left err -> putStrLn ("Error: " ++ err)
    Right tokens -> mapM_ print tokens
  writeFile "output/lexer.h" contents

tokenize :: String -> Either String [Token]
tokenize input = tokenize' 0 input Map.empty -- start with empty macro table

tokenize' :: Int -> String -> MacroTable -> Either String [Token]
tokenize' _ [] _ = Right []
tokenize' pos (c : cs) macros
  | isSpace c = tokenize' (pos + 1) cs macros
  | isDigit c =
      let (digits, rest) = span isDigit (c : cs)
          len = length digits
          num = read digits
          token = Token (TNumber num) (pos, pos + len)
       in do
            rest_tokens <- tokenize' (pos + len) rest macros
            return (token : rest_tokens)
  | isAlpha c || c == '_' =
      let (chars, rest) = span isIdentChar (c : cs)
          len = length chars
          token = Token (TIdent chars) (pos, pos + len)
       in do
            rest_tokens <- tokenize' (pos + len) rest macros
            return (token : rest_tokens)
  | c == '@' =
      let (name, rest) = span isIdentChar cs
          restTrimmed = dropWhile isSpace rest
       in case parseDefinition name of
            Right def ->
              case def of
                Macro ->
                  case parseMacroDefinition restTrimmed of
                    Left err -> Left err
                    Right ((macroName, macroBody), restInput) ->
                      let newMacros = Map.insert macroName macroBody macros
                       in tokenize' (pos + length name + 1) restInput newMacros
                _ ->
                  let len = length name + 1
                      token = Token (TDefinition def) (pos, pos + len)
                   in do
                        rest_tokens <- tokenize' (pos + len) restTrimmed macros
                        return (token : rest_tokens)
            Left _ ->
              case Map.lookup name macros of
                Nothing -> Left ("Unknown macro @" ++ name ++ " at position " ++ show pos)
                Just body -> tokenize' pos (body ++ restTrimmed) macros
    | c == '$' =
        let (name, rest) = span isIdentChar cs
        in if null name
             then Left ("Expected identifier after $ at position " ++ show pos)
             else case Map.lookup name macros of
                    Nothing -> Left ("Unknown macro $" ++ name ++ " at position " ++ show pos)
                    Just body ->
                      -- recursively tokenize the expanded macro body
                      tokenize' pos (body ++ rest) macros
  | c == '%' =
      let (chars, rest) = span isIdentChar cs
       in if null chars
            then Left ("Expected identifier after % at position " ++ show pos)
            else
              let len = length chars + 1
               in case parseModifier chars of
                    Left err -> Left (err ++ " at position " ++ show pos)
                    Right def ->
                      let token = Token (TModifier def) (pos, pos + len)
                       in do
                            rest_tokens <- tokenize' (pos + len) rest macros
                            return (token : rest_tokens)
  | c == '"' = do
      (token, restTokens) <- parseString pos cs
      rest_tokens <- tokenize' (snd (token_span token)) restTokens macros
      return (token : rest_tokens)
  | c == '\'' = do
      (token, restTokens) <- parseChar pos cs
      rest_tokens <- tokenize' (snd (token_span token)) restTokens macros
      return (token : rest_tokens)
  | c == '{' =
      let token = Token TBraceOpen (pos, pos + 1)
       in do
            rest_tokens <- tokenize' (pos + 1) cs macros
            return (token : rest_tokens)
  | c == '}' =
      let token = Token TBraceClose (pos, pos + 1)
       in do
            rest_tokens <- tokenize' (pos + 1) cs macros
            return (token : rest_tokens)
  | c == '(' =
      let token = Token TParanOpen (pos, pos + 1)
       in do
            rest_tokens <- tokenize' (pos + 1) cs macros
            return (token : rest_tokens)
  | c == ')' =
      let token = Token TParanClose (pos, pos + 1)
       in do
            rest_tokens <- tokenize' (pos + 1) cs macros
            return (token : rest_tokens)
  | c == ':' =
      let token = Token TColon (pos, pos + 1)
       in do
            rest_tokens <- tokenize' (pos + 1) cs macros
            return (token : rest_tokens)
  | c == ';' =
      let token = Token TSemicolon (pos, pos + 1)
       in do
            rest_tokens <- tokenize' (pos + 1) cs macros
            return (token : rest_tokens)
  | c == ',' =
      let token = Token TComma (pos, pos + 1)
       in do
            rest_tokens <- tokenize' (pos + 1) cs macros
            return (token : rest_tokens)
  | c == '.' =
      case cs of
        ('.' : rest) ->
          let token = Token TDotDot (pos, pos + 2)
           in do
                rest_tokens <- tokenize' (pos + 2) rest macros
                return (token : rest_tokens)
        _ ->
          let token = Token TDot (pos, pos + 1)
           in do
                rest_tokens <- tokenize' (pos + 1) cs macros
                return (token : rest_tokens)
  | c == '*' =
      let token = Token TStar (pos, pos + 1)
       in do
            rest_tokens <- tokenize' (pos + 1) cs macros
            return (token : rest_tokens)
  | c == '+' =
      let token = Token TPlus (pos, pos + 1)
       in do
            rest_tokens <- tokenize' (pos + 1) cs macros
            return (token : rest_tokens)
  | c == '-' =
      case cs of
        ('>' : rest) ->
          let token = Token TArrow (pos, pos + 2)
           in do
                rest_tokens <- tokenize' (pos + 2) rest macros
                return (token : rest_tokens)
        _ ->
          let token = Token TMinus (pos, pos + 1)
           in do
                rest_tokens <- tokenize' (pos + 1) cs macros
                return (token : rest_tokens)
  | c == '/' =
      case cs of
        ('/' : rest) ->
          let (comment, rest') = span (/= '\n') rest
              len = length comment + 2
           in case rest' of
                [] ->
                  tokenize' (pos + len) [] macros
                (_ : rest'') ->
                  tokenize' (pos + len + 1) rest'' macros
        _ ->
          let token = Token TSlash (pos, pos + 1)
           in do
                rest_tokens <- tokenize' (pos + 1) cs macros
                return (token : rest_tokens)
  | otherwise =
      Left ("Unknown character |" ++ [c] ++ "| at position " ++ show pos)

parseDefinition :: String -> Either String Definition
parseDefinition str =
  case str of
    "keyword" -> Right DKeyword
    "ignore" -> Right DIgnore
    "token" -> Right DToken
    "literal" -> Right DLiteral
    "startwith" -> Right DStartWith
    "comment" -> Right Comment
    "macro" -> Right Macro
    "start_with" -> Right StartWith
    _ -> Left ("Unknown definition @" ++ str)

parseModifier :: String -> Either String Modifier
parseModifier str =
  case str of
    "call_func" -> Right MCallFunc
    "type" -> Right MType
    _ -> Left ("Unknown definition %" ++ str)

parseString :: Int -> String -> Either String (Token, String)
parseString pos cs = go pos [] cs
  where
    go p acc [] = Left ("Unterminated string starting at position " ++ show pos)
    go p acc ('"' : rest) = Right (Token (TString (reverse acc)) (pos, p + 1), rest)
    go p acc ('\\' : c : rest) = go (p + 2) (translateEscape c : acc) rest
    go p acc (c : rest) = go (p + 1) (c : acc) rest

    translateEscape c = case c of
      'n' -> '\n'
      't' -> '\t'
      '\\' -> '\\'
      '"' -> '"'
      '\'' -> '\''
      _ -> c

parseChar :: Int -> String -> Either String (Token, String)
parseChar startPos input = go (startPos + 1) [] input
  where
    go pos acc [] = Left ("Unterminated char starting at position " ++ show startPos)
    go pos acc ('\'' : rest) =
      let len = pos - startPos + 1
       in if length acc /= 1
            then Left ("Char literal must be exactly one character at position " ++ show startPos)
            else Right (Token (TChar (head acc : [])) (startPos, startPos + len), rest)
    go pos acc ('\\' : c : rest) = go (pos + 2) (translateEscape c : acc) rest
    go pos acc (c : rest) = go (pos + 1) (c : acc) rest

    translateEscape c = case c of
      'n' -> '\n'
      't' -> '\t'
      '\\' -> '\\'
      '"' -> '"'
      '\'' -> '\''
      _ -> c

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

parseMacroDefinition :: String -> Either String ((String, String), String)
parseMacroDefinition input =
  let trimmed = dropWhile isSpace input
      (namePart, rest1) = span isIdentChar trimmed
      restBody = dropWhile isSpace rest1
   in if null namePart
        then Left "Expected macro name after @macro"
        else
          let (bodyLines, restInput) = grabMacroBody restBody
           in Right ((namePart, bodyLines), restInput)

grabMacroBody :: String -> (String, String)
grabMacroBody input = go input ""
  where
    go [] acc = (acc, [])
    go s acc =
      let (line, rest) = break (== '\n') s
       in if not (null line) && last line == '\\'
            then go (drop 1 rest) (acc ++ init line ++ "\n") -- remove trailing '\'
            else (acc ++ line, drop 1 rest)

expandMacro :: MacroTable -> String -> Either String String
expandMacro macros name =
  case Map.lookup name macros of
    Just val -> tokenizeMacros macros val -- recursively expand any nested macros
    Nothing -> Left ("Unknown macro @" ++ name)

tokenizeMacros :: MacroTable -> String -> Either String String
tokenizeMacros macros input = go input ""
  where
    go [] acc = Right acc
    go ('@' : rest) acc =
      let (name, rest') = span isIdentChar rest
       in case Map.lookup name macros of
            Nothing -> Left ("Unknown macro @" ++ name)
            Just val -> go rest' (acc ++ val)
    go (c : rest) acc = go rest (acc ++ [c])
