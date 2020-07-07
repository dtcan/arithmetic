import Control.Monad.State.Lazy

data Exceptional t = Success t | Failure String deriving (Show)

data Expr = Literal Int | Sum Expr Expr | Product Expr Expr deriving (Show)
data Program = Empty | Program Expr Program deriving (Show)

printExpr :: Expr -> String
printExpr (Literal x) = show x
printExpr (Sum expr1 expr2) = (printExpr expr1)++"+"++(printExpr expr2)
printExpr (Product expr1 expr2) = (printExpr expr1)++"*"++(printExpr expr2)

data Lexeme = Number String | Operator Char | Semicolon | LeftBracket | RightBracket
instance Show Lexeme where
    show (Number n) = n
    show (Operator c) = [c]
    show Semicolon = ";"
    show LeftBracket = "("
    show RightBracket = ")"
data ParseStackElement = PSExpr Expr | PSLexeme Lexeme
instance Show ParseStackElement where
    show (PSExpr expr) = printExpr expr
    show (PSLexeme lex) = show lex
data ParseState = PARSE_EXPECT_EXPR | PARSE_EXPECT_OP_OR_END | PARSE_SUM_EXPECT_EXPR | PARSE_PROD_EXPECT_EXPR deriving (Show)

numChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

isNumber :: String -> Bool
isNumber "" = False
isNumber (x:"") = elem x numChars
isNumber (x:xs) = (elem x numChars) && (isNumber xs)

eval :: Expr -> Int
eval (Literal x) = x
eval (Sum expr1 expr2) = (eval expr1) + (eval expr2)
eval (Product expr1 expr2) = (eval expr1) * (eval expr2)

run :: Program -> IO ()
run Empty = return ()
run (Program expr prog) = do
    putStrLn $ show $ eval expr
    run prog

parse :: String -> (Exceptional Program)
parse x = (evalState (synParse lexemes) (Empty, [])) where lexemes = (evalState (lexParse x) ([], ""))

lexParse :: String -> State ([Lexeme], String) [Lexeme]
lexParse "" = do
    (lexemes, remaining) <- get
    return $ if (isNumber remaining) then (lexemes ++ [Number remaining]) else lexemes
lexParse (x:xs) = do
        (lexemes, remaining) <- get
        put $ let tempLex = if (isNumber remaining) then [Number remaining] else []
            in case x of
            ';' -> (lexemes ++ tempLex ++ [Semicolon], "")
            '+' -> (lexemes ++ tempLex ++ [Operator x], "")
            '*' -> (lexemes ++ tempLex ++ [Operator x], "")
            '(' -> (lexemes ++ tempLex ++ [LeftBracket], "")
            ')' -> (lexemes ++ tempLex ++ [RightBracket], "")
            _ -> (lexemes, remaining ++ [x])
        lexParse xs

parseInitState = (PARSE_EXPECT_EXPR, [], [])
synParse :: [Lexeme] -> State (Program, [ParseStackElement]) (Exceptional Program)
synParse [] = do
    (program, remaining) <- get
    return $ case remaining of
        [] -> Success program
        _ -> case (evalState (exprParse remaining) parseInitState) of
            Success expr -> evalState (synParse []) (appendExpr program expr, [])
            Failure error -> Failure ("Error while parsing '"++(foldl (\x y -> x++(show y)) "" remaining)++"': "++error)
synParse (Semicolon:xs) = do
    (program, remaining) <- get
    return $ case (evalState (exprParse remaining) parseInitState) of
        Success expr -> evalState (synParse xs) (appendExpr program expr, [])
        Failure error -> Failure ("Error while parsing '"++(foldl (\x y -> x++(show y)) "" remaining)++";': "++error)
synParse (x:xs) = do
    (program, remaining) <- get
    put (program, remaining ++ [PSLexeme x])
    synParse xs

appendExpr :: Program -> Expr -> Program
appendExpr Empty expr = Program expr Empty
appendExpr (Program expr1 prog) expr2 = Program expr1 (appendExpr prog expr2)

findSubexpression :: [ParseStackElement] -> State (Int, [ParseStackElement]) (Exceptional ([ParseStackElement], [ParseStackElement]))
findSubexpression [] = return (Failure "")
findSubexpression (x:xs) = do
    (count, subExpr) <- get
    return $ case x of
        (PSLexeme LeftBracket) -> evalState (findSubexpression xs) (count + 1, subExpr++[x])
        (PSLexeme RightBracket) -> if count == 0
            then (Success (subExpr, xs))
            else evalState (findSubexpression xs) (count - 1, subExpr++[x])
        _ -> evalState (findSubexpression xs) (count, subExpr++[x])

exprParse :: [ParseStackElement] -> State (ParseState, [ParseState], [ParseStackElement]) (Exceptional Expr)
exprParse [] = do
    (state, reduceStack, parseStack) <- get
    case (state, reduceStack, parseStack) of
        (PARSE_EXPECT_OP_OR_END, [], (PSExpr expr):[]) -> return (Success expr)
        (PARSE_EXPECT_OP_OR_END, reduceTop:reduceRest, (PSExpr expr):parseRest) -> return (evalState (exprParse [PSExpr expr]) (reduceTop, reduceRest, parseRest))
        _ -> return (Failure "Unexpected end of expression")
exprParse (x:xs) = do
    (state, reduceStack, parseStack) <- get
    return $ case state of
        PARSE_EXPECT_EXPR -> case x of
            (PSLexeme (Number n)) -> case reduceStack of
                [] -> evalState (exprParse ((PSExpr (Literal (read n))):xs)) (PARSE_EXPECT_EXPR, [], parseStack)
                (reduceTop:reduceRest) -> evalState (exprParse ((PSExpr (Literal (read n))):xs)) (reduceTop, reduceRest, parseStack)
            (PSExpr expr) -> evalState (exprParse xs) (PARSE_EXPECT_OP_OR_END, reduceStack, x:parseStack)
            (PSLexeme LeftBracket) -> case (evalState (findSubexpression xs) (0, [])) of
                Success (subExpr, rest) -> case (evalState (exprParse subExpr) parseInitState) of
                    Success expr -> case reduceStack of
                        [] -> evalState (exprParse ((PSExpr expr):rest)) (PARSE_EXPECT_EXPR, [], parseStack)
                        (reduceTop:reduceRest) -> evalState (exprParse ((PSExpr expr):rest)) (reduceTop, reduceRest, parseStack)
                    Failure error -> (Failure error)
                Failure error -> (Failure "Unexpected end of expression, was expecting )")
            (PSLexeme lex) -> (Failure ("Unexpected "++(show lex)++", was expecting expression"))
        PARSE_EXPECT_OP_OR_END -> case x of
            (PSLexeme (Operator '+')) -> evalState (exprParse xs) (PARSE_EXPECT_EXPR, PARSE_SUM_EXPECT_EXPR:reduceStack, x:parseStack)
            (PSLexeme (Operator '*')) -> evalState (exprParse xs) (PARSE_EXPECT_EXPR, PARSE_PROD_EXPECT_EXPR:reduceStack, x:parseStack)
            _ -> (Failure ("Unexpected "++(show x)++", was expecting an operator or end of expression"))
        PARSE_SUM_EXPECT_EXPR -> case x of
            (PSExpr expr) -> case (reduceStack, parseStack) of
                ([], (PSLexeme (Operator '+')):((PSExpr expr2):parseRest)) -> evalState (exprParse ((PSExpr (Sum expr2 expr)):xs)) (PARSE_EXPECT_EXPR, [], parseRest)
                (reduceTop:reduceRest, (PSLexeme (Operator '+')):((PSExpr expr2):parseRest)) -> evalState (exprParse ((PSExpr (Sum expr2 expr)):xs)) (reduceTop, reduceRest, parseRest)
                _ -> (Failure ("Unexpected "++(show expr)++", missing +"))
            (PSLexeme lex) -> (Failure ("Unexpected "++(show lex)++", was expecting 2nd expression in sum"))
        PARSE_PROD_EXPECT_EXPR -> case x of
            (PSExpr expr) -> case (reduceStack, parseStack) of
                ([], (PSLexeme (Operator '*')):((PSExpr expr2):parseRest)) -> evalState (exprParse ((PSExpr (Product expr2 expr)):xs)) (PARSE_EXPECT_EXPR, [], parseRest)
                (reduceTop:reduceRest, (PSLexeme (Operator '*')):((PSExpr expr2):parseRest)) -> evalState (exprParse ((PSExpr (Product expr2 expr)):xs)) (reduceTop, reduceRest, parseRest)
                _ -> (Failure ("Unexpected "++(show expr)++", missing *"))
            (PSLexeme lex) -> (Failure ("Unexpected "++(show lex)++", was expecting 2nd expression in product"))


main :: IO ()
main = do
    source <- getLine
    case (parse source) of
        Success prog -> run prog
        Failure error -> putStrLn error
    main
