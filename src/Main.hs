module Main where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Data.Map as Map

data Position = Position
    { line :: Int
    , column :: Int
    , offset :: Int
    }
    deriving (Show, Eq)

data Span = Span
    { start :: Position
    , end :: Position
    , origin :: Origin
    }
    deriving (Show, Eq)

data Origin
    = Original
    | MacroExpansion String Span
    deriving (Show, Eq)

advance :: Position -> Char -> Position
advance pos c =
    case c of
        '\n' -> Position (line pos + 1) 1 (offset pos + 1)
        _ -> Position (line pos) (column pos + 1) (offset pos + 1)

advanceMany :: Position -> String -> Position
advanceMany = foldl advance

data ErrorType
    = LexError
    | ParseError
    | SemanticError
    deriving (Show)

data CompileError = CompileError
    { errType :: ErrorType
    , errMsg :: String
    , errSpan :: Span
    }
    deriving (Show)

lexError :: Position -> String -> CompileError
lexError pos msg =
    CompileError LexError msg (Span pos pos Original)

withOrigin :: Origin -> Span -> Span
withOrigin o sp = sp{origin = o}

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

data Modifier = MCallFunc | MType | MReserved | MRegex deriving (Show)

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
    | TOr
    | TBinOr
    | TAnd
    | TBinAnd
    | TEOF
    deriving (Show)

data Token = Token
    { token_type :: TokenType
    , token_span :: Span
    }
    deriving (Show)

main :: IO ()
main = do
    contents <- readFile "input/lex.yapper"
    case tokenize contents of
        Left err -> putStrLn (prettyError contents err)
        Right tokens -> mapM_ print tokens

tokenize :: String -> Either CompileError [Token]
tokenize input = tokenize' (Position 1 1 0) input Map.empty

tokenize' :: Position -> String -> MacroTable -> Either CompileError [Token]
tokenize' _ [] _ = Right []
tokenize' pos (c : cs) macros
    | isSpace c =
        tokenize' (advance pos c) cs macros
    | isDigit c =
        let (digits, rest) = span isDigit (c : cs)
            endPos = advanceMany pos digits
            token = Token (TNumber (read digits)) (Span pos endPos Original)
         in (token :) <$> tokenize' endPos rest macros
    | isAlpha c || c == '_' =
        let (chars, rest) = span isIdentChar (c : cs)
            endPos = advanceMany pos chars
            token = Token (TIdent chars) (Span pos endPos Original)
         in (token :) <$> tokenize' endPos rest macros
    | c == '@' =
        let fullInput = c : cs
            (name, rest) = span isIdentChar cs
            defText = '@' : name
            endPos = advanceMany pos defText
            restTrimmed = dropWhile isSpace rest
         in case parseDefinition name of
                Right def ->
                    case def of
                        Macro ->
                            case parseMacroDefinition restTrimmed of
                                Left err -> Left (lexError pos err)
                                Right ((macroName, macroBody), restInput) ->
                                    let newMacros = Map.insert macroName macroBody macros
                                        consumed = take (length fullInput - length restInput) fullInput
                                        newPos = advanceMany pos consumed
                                     in tokenize' newPos restInput newMacros
                        _ ->
                            let token = Token (TDefinition def) (Span pos endPos Original)
                             in (token :) <$> tokenize' endPos restTrimmed macros
                Left _ ->
                    case Map.lookup name macros of
                        Nothing ->
                            let sp = Span pos endPos Original
                             in Left (CompileError LexError ("Unknown definition " ++ defText) sp)
                        Just body ->
                            let newOrigin = MacroExpansion name (Span pos endPos Original)
                             in do
                                    tokens <- tokenize' pos body macros
                                    let tokens' = map (\t -> t{token_span = withOrigin newOrigin (token_span t)}) tokens
                                    restTokens <- tokenize' endPos restTrimmed macros
                                    return (tokens' ++ restTokens)
    | c == '$' =
        let (name, rest) = span isIdentChar cs
            callText = '$' : name
            endPos = advanceMany pos callText
         in if null name
                then Left (lexError pos "Expected identifier after '$'")
                else case Map.lookup name macros of
                    Nothing ->
                        let sp = Span pos endPos Original
                         in Left (CompileError LexError ("Unknown macro " ++ callText) sp)
                    Just body ->
                        let newOrigin = MacroExpansion name (Span pos endPos Original)
                         in do
                                tokens <- tokenize' pos body macros
                                let tokens' = map (\t -> t{token_span = withOrigin newOrigin (token_span t)}) tokens
                                restTokens <- tokenize' endPos rest macros
                                return (tokens' ++ restTokens)
    | c == '%' =
        let (name, rest) = span isIdentChar cs
            modText = '%' : name
            endPos = advanceMany pos modText
         in if null name
                then Left (lexError pos "Expected identifier after '%'")
                else case parseModifier name of
                    Left err -> Left (lexError pos err)
                    Right def ->
                        let token = Token (TModifier def) (Span pos endPos Original)
                         in (token :) <$> tokenize' endPos rest macros
    | c == '"' = do
        (token, restTokens, newPos) <- parseString pos cs
        (token :) <$> tokenize' newPos restTokens macros
    | c == '\'' = do
        (token, restTokens, newPos) <- parseChar pos cs
        (token :) <$> tokenize' newPos restTokens macros
    | otherwise = case c of
        '{' -> single TBraceOpen
        '}' -> single TBraceClose
        '(' -> single TParanOpen
        ')' -> single TParanClose
        ':' -> single TColon
        ';' -> single TSemicolon
        ',' -> single TComma
        '.' -> case cs of
            ('.' : rest) ->
                let endPos = advanceMany pos ".."
                 in (Token TDotDot (Span pos endPos Original) :) <$> tokenize' endPos rest macros
            _ -> single TDot
        '*' -> single TStar
        '+' -> single TPlus
        '-' -> case cs of
            ('>' : rest) ->
                let endPos = advanceMany pos "->"
                 in (Token TArrow (Span pos endPos Original) :) <$> tokenize' endPos rest macros
            _ -> single TMinus
        '/' -> case cs of
            ('/' : rest) ->
                let (comment, rest') = span (/= '\n') rest
                    newPos = advanceMany pos ("//" ++ comment)
                 in case rest' of
                        [] -> tokenize' newPos [] macros
                        (_ : rest'') -> tokenize' (advance newPos '\n') rest'' macros
            _ -> single TSlash
        '|' -> case cs of
            ('|' : rest) ->
                let endPos = advanceMany pos "||"
                 in (Token TOr (Span pos endPos Original) :) <$> tokenize' endPos rest macros
            _ -> single TBinOr
        '&' -> case cs of
            ('&' : rest) ->
                let endPos = advanceMany pos "||"
                 in (Token TAnd (Span pos endPos Original) :) <$> tokenize' endPos rest macros
            _ -> single TBinAnd
        _ -> Left (lexError pos ("Unknown character '" ++ [c] ++ "'"))
  where
    single t =
        let newPos = advance pos c
         in (Token t (Span pos newPos Original) :) <$> tokenize' newPos cs macros

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
        "reserved" -> Right MReserved
        "regex" -> Right MRegex
        _ -> Left ("Unknown definition %" ++ str)

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

parseString ::
    Position ->
    String ->
    Either CompileError (Token, String, Position)
parseString startPos = go (advance startPos '"') []
  where
    go _ _ [] =
        Left (lexError startPos "Unterminated string")
    go pos acc ('"' : rest) =
        let endPos = advance pos '"'
         in Right (Token (TString (reverse acc)) (Span startPos endPos Original), rest, endPos)
    go pos acc ('\\' : c : rest) =
        let pos' = advance (advance pos '\\') c
         in go pos' (translateEscape c : acc) rest
    go pos acc (c : rest) =
        go (advance pos c) (c : acc) rest

parseChar ::
    Position ->
    String ->
    Either CompileError (Token, String, Position)
parseChar startPos = go (advance startPos '\'') []
  where
    go _ _ [] =
        Left (lexError startPos "Unterminated char literal")
    go pos acc ('\'' : rest)
        | length acc /= 1 =
            Left (lexError startPos "Char literal must be exactly one character")
        | otherwise =
            let endPos = advance pos '\''
             in Right (Token (TChar acc) (Span startPos endPos Original), rest, endPos)
    go pos _ ('\\' : c : rest) =
        let pos' = advance (advance pos '\\') c
         in go pos' [translateEscape c] rest
    go pos _ (c : rest) =
        go (advance pos c) [c] rest

translateEscape :: Char -> Char
translateEscape c =
    case c of
        'n' -> '\n'
        't' -> '\t'
        '\\' -> '\\'
        '"' -> '"'
        '\'' -> '\''
        _ -> c

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
grabMacroBody input =
    let (body, rest) = break (== '\n') input
     in (body, drop 1 rest)

prettyError :: String -> CompileError -> String
prettyError src err =
    let Span (Position l cStart _) (Position _ cEnd _) _ = errSpan err
        ls = lines src
        lineText =
            if l <= length ls then ls !! (l - 1) else ""
        width = max 1 (cEnd - cStart)
     in show (errType err)
            ++ " at "
            ++ show l
            ++ ":"
            ++ show cStart
            ++ "\n"
            ++ lineText
            ++ "\n"
            ++ replicate (cStart - 1) ' '
            ++ replicate width '^'
            ++ "\n"
            ++ errMsg err
