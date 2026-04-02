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

-------------------- Lexer

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
    | DComment
    | DMacro
    deriving (Show)

data Modifier
    = MCallFunc
    | MSlice
    | MReserved
    | MRegex
    | MPattern
    | MCount
    deriving (Show, Ord, Eq)

data TokenType
    = TNumber Int
    | TString String
    | TDefinition Definition
    | TIdent String
    | TChar String
    | TModifier Modifier
    | TBraceOpen
    | TBraceClose
    | TParenOpen
    | TParenClose
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
    | TQuestion
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
        Right tokens -> do
            case parseDefinitions tokens of
                Left parseErr -> putStrLn (prettyError contents parseErr)
                Right defTable -> do
                    putStrLn "Parsed definitions:"
                    mapM_ printDefinition (Map.toList defTable)

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
                        DMacro ->
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
        '(' -> single TParenOpen
        ')' -> single TParenClose
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
                let endPos = advanceMany pos "&&"
                 in (Token TAnd (Span pos endPos Original) :) <$> tokenize' endPos rest macros
            _ -> single TBinAnd
        '>' -> single TQuestion
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
        "comment" -> Right DComment
        "macro" -> Right DMacro
        "start_with" -> Right DStartWith
        _ -> Left ("Unknown definition @" ++ str)

parseModifier :: String -> Either String Modifier
parseModifier str =
    case str of
        "call_func" -> Right MCallFunc
        "slice" -> Right MSlice
        "reserved" -> Right MReserved
        "regex" -> Right MRegex
        "pattern" -> Right MPattern
        "count" -> Right MCount
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

-------------------- Parser

type DefinitionTable = Map.Map String ParsedDefinition

data ParsedDefinition = ParsedDefinition
    { defType :: Definition
    , params :: [Param]
    , pdSpan :: Span
    }
    deriving (Show)

data Param
    = ParIdent String
    | ParString String
    | ParNumber Int
    | ParChar String
    | ParModifier Modifier
    | ParPattern Pattern
    | ParCount String String
    | ParSlice (Maybe String)
    deriving (Show)

data Pattern
    = PSeq Pattern Pattern
    | POr Pattern Pattern
    | PStar Pattern
    | PPlus Pattern
    | POptional Pattern
    | PBind String Pattern
    | PStringLit String
    | PRegexLit String
    | PSlice (Maybe String) Pattern
    | PCount String String Pattern
    deriving (Show)

parseDefinitions :: [Token] -> Either CompileError DefinitionTable
parseDefinitions tokens = go tokens Map.empty
  where
    go [] table = Right table
    go (t : ts) table =
        case token_type t of
            TDefinition def ->
                case extractParams ts of
                    Left err -> Left err
                    Right (paramList, rest) ->
                        let pd = ParsedDefinition{defType = def, params = paramList, pdSpan = token_span t}
                            key = definitionKey def paramList
                         in go rest (Map.insert key pd table)
            _ -> Left $ CompileError ParseError ("Unexpected token: " ++ show t) (token_span t)

extractParams :: [Token] -> Either CompileError ([Param], [Token])
extractParams (Token TParenOpen spanOpen : ts) = go ts []
  where
    go :: [Token] -> [Param] -> Either CompileError ([Param], [Token])
    go [] _ = Left $ CompileError ParseError "Unterminated parameter list" spanOpen
    go (Token t spanT : rest) acc =
        case t of
            TParenClose -> Right (reverse acc, rest)
            TComma -> go rest acc
            TIdent s -> nextParam (ParIdent s) rest acc spanT
            TString s -> nextParam (ParString s) rest acc spanT
            TChar s -> nextParam (ParChar s) rest acc spanT
            TNumber n -> nextParam (ParNumber n) rest acc spanT
            TModifier m ->
                case Map.lookup m modifierParsers of
                    Nothing -> Left $ CompileError ParseError ("No parser for modifier: " ++ show m) spanT
                    Just parser -> do
                        (parsedParams, rest') <- parser (Token t spanT : rest)
                        go rest' (acc ++ parsedParams)
            _ ->
                Left $
                    CompileError
                        ParseError
                        ("Unexpected token in parameter list: " ++ show t)
                        spanT

    nextParam :: Param -> [Token] -> [Param] -> Span -> Either CompileError ([Param], [Token])
    nextParam _ [] _ _ =
        Left $ CompileError ParseError "Unterminated parameter list" spanOpen
    nextParam param (Token nextT nextSpan : rest') acc _ =
        case nextT of
            TComma -> go rest' (param : acc)
            TParenClose -> Right (reverse (param : acc), rest')
            _ -> Left $ CompileError ParseError ("Expected ',' or ')' after parameter, got: " ++ show nextT) nextSpan
extractParams ts = Right ([], ts)

definitionKey :: Definition -> [Param] -> String
definitionKey _ (ParIdent name : _) = name
definitionKey _ (ParString name : _) = name
definitionKey _ (ParChar name : _) = name
definitionKey _ (ParPattern p : _) = show p
definitionKey _ (ParSlice p : _) = show p
definitionKey _ (ParNumber n : _) = show n
definitionKey _ (ParCount name var : _) = name ++ "':" ++ var
definitionKey _ (ParModifier m : _) = show m
definitionKey def [] = show def

printDefinition :: (String, ParsedDefinition) -> IO ()
printDefinition (key, pd) =
    putStrLn $ key ++ " => " ++ show (defType pd) ++ " " ++ show (params pd)

type ModifierParser = [Token] -> Either CompileError ([Param], [Token])
modifierParsers :: Map.Map Modifier ModifierParser
modifierParsers =
    Map.fromList
        [ (MCallFunc, parseCallFunc)
        , (MSlice, parseSlice)
        , (MReserved, parseReserved)
        , (MRegex, parseRegex)
        , (MPattern, parsePattern)
        ]

parseCallFunc :: ModifierParser
parseCallFunc (Token (TModifier MCallFunc) modSpan : ts) = parseAfterModifier ts
  where
    parseAfterModifier :: [Token] -> Either CompileError ([Param], [Token])
    parseAfterModifier (Token TParenOpen parenSpan : rest) = do
        (extractedParams, restTokens) <- extractParams (Token TParenOpen parenSpan : rest)
        case extractedParams of
            [ParIdent funcName] -> Right ([ParModifier MCallFunc, ParIdent funcName], restTokens)
            _ ->
                Left $
                    CompileError
                        ParseError
                        "Expected single identifier inside %call_func(...)"
                        (Span (lineStart modSpan) (lineEnd modSpan) Original)
              where
                lineStart (Span startPos _ _) = startPos
                lineEnd (Span _ endPos _) = endPos
    parseAfterModifier (t : _) =
        Left $ CompileError ParseError ("Expected '(' after %call_func, got: " ++ show (token_type t)) (token_span t)
    parseAfterModifier [] =
        Left $ CompileError ParseError "Unexpected end of input after %call_func" modSpan
parseCallFunc (t : _) =
    Left $ CompileError ParseError ("Expected %call_func modifier, got: " ++ show (token_type t)) (token_span t)
parseCallFunc [] =
    Left $ CompileError ParseError "Unexpected end of input while parsing %call_func" (Span (Position 0 0 0) (Position 0 0 0) Original)

parseRegex :: ModifierParser
parseRegex (Token (TModifier MRegex) modSpan : ts) =
    case ts of
        (Token (TString str) _ : rest) ->
            Right ([ParModifier MRegex, ParString str], rest)
        (t : _) ->
            Left $
                CompileError
                    ParseError
                    ("Expected string after %regex, got: " ++ show (token_type t))
                    (token_span t)
        [] ->
            Left $
                CompileError
                    ParseError
                    "Unexpected end of input after %regex"
                    modSpan
parseRegex (t : _) =
    Left $
        CompileError
            ParseError
            ("Expected %regex modifier, got: " ++ show (token_type t))
            (token_span t)
parseRegex [] =
    Left $ CompileError ParseError "Unexpected end of input while parsing %regex" (Span (Position 0 0 0) (Position 0 0 0) Original)

parseReserved :: ModifierParser
parseReserved (Token (TModifier MReserved) _ : ts) =
    Right ([ParModifier MReserved], ts)
parseReserved (t : _) =
    Left $
        CompileError
            ParseError
            ("Expected %reserved modifier, got: " ++ show (token_type t))
            (token_span t)
parseReserved [] =
    Left $ CompileError ParseError "Unexpected end of input while parsing %reserved" (Span (Position 0 0 0) (Position 0 0 0) Original)

parseSlice :: ModifierParser
parseSlice (Token (TModifier MSlice) tspan : ts) =
    case ts of
        (Token TParenOpen _ : rest) -> do
            (paramsList, rest') <- extractParams (Token TParenOpen undefined : rest)
            case paramsList of
                [] -> Right ([ParModifier MSlice], rest')
                [ParIdent name] -> Right ([ParModifier MSlice, ParIdent name], rest')
                _ -> Left $ CompileError ParseError "Expected single identifier in %slice(...)" tspan
        _ ->
            Right ([ParModifier MSlice], ts)
parseSlice (t : _) =
    Left $ CompileError ParseError ("Expected %slice, got: " ++ show (token_type t)) (token_span t)
parseSlice [] =
    Left $ CompileError ParseError "Unexpected end of input while parsing %slice" (Span (Position 0 0 0) (Position 0 0 0) Original)

parsePattern :: ModifierParser
parsePattern (Token (TModifier MPattern) tspan : Token TParenOpen _ : ts) = do
    (pat, rest') <- parsePatternExprUntilParen ts
    case rest' of
        (Token TParenClose _ : rest'') -> Right ([ParPattern pat], rest'')
        (t : _) -> Left $ CompileError ParseError "Expected ')' after %pattern" (token_span t)
        [] -> Left $ CompileError ParseError "Unterminated %pattern" tspan
parsePattern (Token (TModifier MPattern) tspan : _) =
    Left $ CompileError ParseError "Expected '(' after %pattern" tspan
parsePattern _ =
    Left $ CompileError ParseError "Invalid %pattern usage" (Span (Position 0 0 0) (Position 0 0 0) Original)

parsePatternExprUntilParen :: [Token] -> Either CompileError (Pattern, [Token])
parsePatternExprUntilParen tokens = parseTokens tokens []
  where
    parseTokens :: [Token] -> [Pattern] -> Either CompileError (Pattern, [Token])
    parseTokens [] _ = Left $ CompileError ParseError "Unterminated %pattern expression" (Span (Position 0 0 0) (Position 0 0 0) Original)
    parseTokens (Token TParenClose _ : rest) acc =
        Right (foldl1 PSeq acc, Token TParenClose undefined : rest)
    parseTokens (Token (TString s) _ : rest) acc =
        parseTokens rest (acc ++ [PStringLit s])
    parseTokens (Token (TModifier MRegex) _ : Token (TString s) _ : rest) acc =
        parseTokens rest (acc ++ [PRegexLit s])
    parseTokens (Token (TChar c) _ : rest) acc =
        parseTokens rest (acc ++ [PStringLit c])
    parseTokens (Token (TModifier MCount) tspan : Token TParenOpen _ : rest) acc = do
        (paramsList, rest') <- extractParams (Token TParenOpen undefined : rest)
        case paramsList of
            [ParIdent name, ParIdent var] -> do
                (inner, rest'') <- parseTokens rest' []
                parseTokens rest'' (acc ++ [PCount name var inner])
            _ -> Left $ CompileError ParseError "Expected %count(name,var)" tspan
    parseTokens (Token (TModifier MSlice) _ : rest) acc = do
        case rest of
            (Token TParenOpen _ : ts) -> do
                (paramsList, rest') <- extractParams (Token TParenOpen undefined : ts)
                let sliceName = case paramsList of
                        [ParIdent name] -> Just name
                        [] -> Nothing
                        _ -> Nothing
                (inner, rest'') <- parseTokens rest' []
                parseTokens rest'' (acc ++ [PSlice sliceName inner])
            _ -> do
                (inner, rest'') <- parseTokens rest []
                parseTokens rest'' (acc ++ [PSlice Nothing inner])
    parseTokens (Token TArrow _ : Token (TIdent name) _ : rest) acc =
        case reverse acc of
            [] -> Left $ CompileError ParseError "Nothing to bind with '->'" (Span (Position 0 0 0) (Position 0 0 0) Original)
            (p : ps) -> parseTokens rest (reverse ps ++ [PSlice (Just name) p])
    parseTokens (Token TArrow _ : rest) acc =
        case reverse acc of
            [] -> Left $ CompileError ParseError "Nothing to bind with '->'" (Span (Position 0 0 0) (Position 0 0 0) Original)
            (p : ps) -> parseTokens rest (reverse ps ++ [PSlice Nothing p])
    parseTokens (Token TBinAnd _ : rest) acc = do
        (right, rest') <- parseTokens rest []
        parseTokens rest' (acc ++ [PSeq (foldl1 PSeq acc) right])
    parseTokens (Token TOr _ : rest) acc = do
        (right, rest') <- parseTokens rest []
        parseTokens rest' [POr (foldl1 PSeq acc) right]
    parseTokens (t : _) _ =
        Left $ CompileError ParseError ("Unexpected token in %pattern: " ++ show (token_type t)) (token_span t)

parsePatternParams :: [Token] -> Either CompileError (Pattern, [Token])
parsePatternParams tokens = do
    expandedTokens <- expandPatternModifiers tokens
    parsePatternExpr expandedTokens

expandPatternModifiers :: [Token] -> Either CompileError [Token]
expandPatternModifiers [] = Right []
expandPatternModifiers (Token (TModifier MCount) tspan : Token TParenOpen _ : ts) = do
    (paramsList, rest) <- extractParams (Token TParenOpen undefined : ts)
    case paramsList of
        [ParIdent _, ParIdent _] -> do
            restTokens <- expandPatternModifiers rest
            Right (Token (TModifier MCount) tspan : restTokens)
        _ -> Left $ CompileError ParseError "Expected %count(name,var)" tspan
expandPatternModifiers (Token (TModifier MSlice) tspan : ts) = do
    restTokens <- expandPatternModifiers ts
    Right (Token (TModifier MSlice) tspan : restTokens)
expandPatternModifiers (t : ts) = do
    restTokens <- expandPatternModifiers ts
    Right (t : restTokens)

parsePatternExpr :: [Token] -> Either CompileError (Pattern, [Token])
parsePatternExpr = parseSeq

parseSeq :: [Token] -> Either CompileError (Pattern, [Token])
parseSeq tokens = do
    (left, rest) <- parseOr tokens
    go left rest
  where
    go left (Token TBinAnd _ : ts) = do
        (right, rest) <- parseOr ts
        go (PSeq left right) rest
    go left ts = Right (left, ts)

parseOr :: [Token] -> Either CompileError (Pattern, [Token])
parseOr tokens = do
    (left, rest) <- parseRepeat tokens
    go left rest
  where
    go left (Token TOr _ : ts) = do
        (right, rest) <- parseRepeat ts
        go (POr left right) rest
    go left ts = Right (left, ts)

parseRepeat :: [Token] -> Either CompileError (Pattern, [Token])
parseRepeat tokens = do
    (atom, rest) <- parseAtom tokens
    applyRepeat atom rest

applyRepeat :: Pattern -> [Token] -> Either CompileError (Pattern, [Token])
applyRepeat p (Token TStar _ : ts) = applyRepeat (PStar p) ts
applyRepeat p (Token TPlus _ : ts) = applyRepeat (PPlus p) ts
applyRepeat p (Token TQuestion _ : ts) = applyRepeat (POptional p) ts
applyRepeat p ts = applyArrowOrBind p ts

applyArrowOrBind :: Pattern -> [Token] -> Either CompileError (Pattern, [Token])
applyArrowOrBind p (Token TArrow _ : Token (TIdent name) _ : ts) =
    Right (PSlice (Just name) p, ts)
applyArrowOrBind p (Token TArrow _ : ts) =
    Right (PSlice Nothing p, ts)
applyArrowOrBind p (Token (TIdent name) _ : ts) =
    Right (PBind name p, ts)
applyArrowOrBind p ts = Right (p, ts)

parseAtom :: [Token] -> Either CompileError (Pattern, [Token])
parseAtom (Token (TString s) _ : ts) = Right (PStringLit s, ts)
parseAtom (Token (TModifier MRegex) _ : Token (TString s) _ : ts) = Right (PRegexLit s, ts)
parseAtom (Token TParenOpen _ : ts) = do
    (p, rest) <- parsePatternExpr ts
    case rest of
        (Token TParenClose _ : rest') -> Right (p, rest')
        (t : _) -> Left $ CompileError ParseError "Expected ')'" (token_span t)
        [] -> Left $ CompileError ParseError "Unterminated group" (Span (Position 0 0 0) (Position 0 0 0) Original)
parseAtom (Token (TModifier MCount) tspan : ts) =
    case ts of
        (Token TParenOpen _ : rest) -> do
            (paramsList, rest') <- extractParams (Token TParenOpen undefined : rest)
            case paramsList of
                [ParIdent name, ParIdent var] -> do
                    (inner, rest'') <- parseAtom rest'
                    Right (PCount name var inner, rest'')
                _ -> Left $ CompileError ParseError "Expected %count(name,var)" tspan
        _ -> Left $ CompileError ParseError "Expected '(' after %count" tspan
parseAtom (Token (TModifier MSlice) _ : ts) =
    case ts of
        (Token TParenOpen _ : rest) -> do
            (paramsList, rest') <- extractParams (Token TParenOpen undefined : rest)
            let sliceName = case paramsList of
                    [ParIdent name] -> Just name
                    [] -> Nothing
                    _ -> Nothing
            (inner, rest'') <- parseAtom rest'
            Right (PSlice sliceName inner, rest'')
        _ -> do
            (inner, rest'') <- parseAtom ts
            Right (PSlice Nothing inner, rest'')
parseAtom (t : _) = Left $ CompileError ParseError ("Invalid pattern atom: " ++ show (token_type t)) (token_span t)
parseAtom [] = Left $ CompileError ParseError "Unexpected end of input in pattern" (Span (Position 0 0 0) (Position 0 0 0) Original)
