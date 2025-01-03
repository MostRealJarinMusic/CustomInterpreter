module Main where
import Prelude hiding (lookup)
import Control.Monad ( unless )
import Data.Map ( Map, insert, lookup, empty )
import Definitions

main :: IO ()
main = do
  evaluateProgram testProgram
  return ()

testProgram :: Expr
--testProgram = Seq (Set "x" (Lit (VInt 7))) (Seq (Set "y" (Lit (VString "6"))) (Set "z" (BinOp Add (Get "x") (Get "y"))))
--testProgram = Set "x" (BinOp Add (Lit (VInt 7)) (Lit (VString "6")))
--testProgram = Seq (Set "x" (Lit (VInt 42))) (Print (Get "x"))
--testProgram = Seq (Set "msg" (Lit (VString "Hello"))) (Seq (Set "count" (Lit (VInt 3))) (While (BinOp Gt (Get "count") (Lit (VInt 0))) (Seq (Print (Get "msg")) (Set "count" (BinOp Sub (Get "count") (Lit (VInt 1)))))))
--testProgram = Seq (Set "x" (Lit (VInt 10))) (While (BinOp Gt (Get "x") (Lit (VInt 0))) (Seq (Set "x" (BinOp Sub (Get "x") (Lit (VInt 1)))) (Print (Get "x"))))
--testProgram = Print (Get "undefinedVar")
--testProgram = Seq (Set "msg" (Lit (VString "Hello"))) (Seq (Set "count" (Lit (VInt 10))) (DoWhile (BinOp Gt (Get "count") (Lit (VInt 0))) (Seq (Print (Get "msg")) (Set "count" (BinOp Sub (Get "count") (Lit (VInt 1)))) )))
testProgram = 
  Seq 
    [ Set "msg" (Lit (VString "Hello"))
    , Set "count" (Lit (VInt 10))
    , DoWhile 
        (BinOp Gt (Get "count") (Lit (VInt 0))) 
        (Seq 
            [ Print (Get "msg")
            , Set "count" (BinOp Sub (Get "count") (Lit (VInt 1)))
            ]
        )
    ]

{-
tokenizer :: String -> [Token]
tokenizer = lexer [] . words
 where 
    lexer :: [Token] -> [String] -> [Token]
    lexer acc [] = acc ++ [TokEOF]
    lexer acc (x:xs) 
      | x == ";"                                                                              = lexer (acc ++ [TokSemicolon]) xs
      | x == "("                                                                              = lexer (acc ++ [TokLeftBracket]) xs
      | x == ")"                                                                              = lexer (acc ++ [TokRightBracket]) xs
      | x == "{"                                                                              = lexer (acc ++ [TokLeftBrace]) xs
      | x == "}"                                                                              = lexer (acc ++ [TokRightBrace]) xs
      | all (`elem` "0123456789") x                                                           = lexer (acc ++ [TokInt (read x)]) xs
      | x `elem` ["True", "False"]                                                            = lexer (acc ++ [TokBool (x == "True")]) xs
      | x `elem` ["if", "else", "while", "do", "print", "skip"]                               = lexer (acc ++ [TokKeyword x]) xs
      | x `elem` ["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||", "!"] = lexer (acc ++ [TokSymbol x]) xs
      | head x == '"' && last x == '"'                                                        = lexer (acc ++ [TokString ((init . tail) x)]) xs
      | otherwise                                                                             = lexer (acc ++ [TokIdentifier x]) xs

-}

evaluateProgram :: Expr -> IO (Value, Variables)
evaluateProgram = evaluate empty 

evaluate :: Variables -> Expr -> IO (Value, Variables)
evaluate vars (Lit value) = return (value, vars)

evaluate vars (Set name expr) = do
    case typecheck vars expr of 
        Left str -> do
            putStrLn $ "Type error in Set for variable '" ++ name ++ "': " ++ str
            return (VError, vars)
        Right exprType -> do
            case lookup name vars of
                Just (existingType, _) | existingType /= exprType -> do
                    putStrLn $ "Type mismatch for variable '" ++ name ++ "': "
                        ++ "Expected " ++ show existingType ++ ", got " ++ show exprType
                    return (VError, vars)
                _ -> do
                    (val, _) <- evaluate vars expr
                    if val == VError
                    then return (VError, vars)    
                    else return (VNone, insert name (exprType, val) vars)

evaluate vars (Get name) = case lookup name vars of
    Nothing -> do
        putStrLn $ "Variable " ++ name ++ " not defined within the current scope"
        return (VError, vars)
    Just (_, val) -> return (val, vars)

evaluate vars (BinOp op expr1 expr2) = checkThenEval vars (BinOp op expr1 expr2) evaluate'
    where  
        evaluate' = do
            (value1, vars') <- evaluate vars expr1
            (value2, vars'') <- evaluate vars' expr2
            if value1 == VError || value2 == VError
            then return (VError, vars'')
            else do
                let result = evalBinOps op value1 value2
                case result of
                    Left str -> do
                        putStrLn str
                        return (VError, vars'')
                    Right validResult -> return (validResult, vars'')

evaluate vars (UnOp op expr) = checkThenEval vars (UnOp op expr) evaluate'
    where
        evaluate' = do
            (value, vars') <- evaluate vars expr
            if value == VError 
            then return (VError, vars')
            else do
                let result = evalUnOps op value
                case result of
                    Left str -> do
                        putStrLn str
                        return (VError, vars')
                    Right validResult -> return (validResult, vars')

evaluate vars (IfElse pred trueExpr falseExpr) = 
    checkThenEval vars (IfElse pred trueExpr falseExpr) evaluate'
    where
        evaluate' = do
            (predicateValue, vars') <- evaluate vars pred
            if predicateValue == VError 
            then return (VError, vars')
            else case predicateValue of 
                VBool True -> evaluate vars' trueExpr
                VBool False -> evaluate vars' falseExpr

evaluate vars (Seq [])         = return (VNone, vars)
evaluate vars (Seq (expr:exprs)) = checkThenEval vars expr evaluate'
    where 
        evaluate' = do
            (value, vars') <- evaluate vars expr
            case value of 
                VNone -> evaluate vars' (Seq exprs)
                _     -> do
                    putStrLn "Unexpected value in sequence"
                    return (VError, vars')

evaluate vars (While pred expr) = checkThenEval vars (While pred expr) evaluate'
    where 
        evaluate' = do
            (predicateValue, vars') <- evaluate vars pred
            case predicateValue of 
                VError      -> return (VError, vars')
                VBool False -> return (VNone, vars')
                _ -> do
                    (_, vars'') <- evaluate vars' expr
                    evaluate vars'' (While pred expr)
    
evaluate vars (DoWhile pred expr) = checkThenEval vars (DoWhile pred expr) evaluate'
    where
        evaluate' = do
            (value, vars') <- evaluate vars expr
            if value /= VNone
            then return (VError, vars')
            else do     
                (predicateValue, vars'') <- evaluate vars' pred
                case predicateValue of 
                    VError      -> return (VError, vars')
                    VBool False -> return (VNone, vars'')
                    _ -> evaluate vars'' (DoWhile pred expr)

evaluate vars Skip = return (VNone, vars)

evaluate vars (Print expr) = checkThenEval vars (Print expr) evaluate'
    where
        evaluate' = do
            (value, vars') <- evaluate vars expr
            case value of
                VError -> return (VError, vars)
                _      -> do
                    putStrLn (prettyPrint value)
                    return (VNone, vars)



--Helper for printing
prettyPrint :: Value -> String
prettyPrint (VBool b) = if b then "True" else "False"
prettyPrint (VString str) = "\"" ++ str ++ "\""
prettyPrint (VInt n) = show n
prettyPrint VNone = ""


--Helper for typechecking
checkThenEval :: Variables -> Expr -> IO (Value, Variables) -> IO (Value, Variables)
checkThenEval vars testExpr eval = do
    --print ("About to typecheck " ++ show testExpr)
    let checkedType = typecheck vars testExpr
    case checkedType of
        Left str -> do
            putStrLn str
            return (VError, vars)
        Right _ -> eval


--Helper functions for evaluation
evalUnOps :: UnOp -> Value -> Either String Value
evalUnOps Neg (VInt x) = Right $ VInt (negate x)
evalUnOps Neg _        = Left "Type error: incorrect type for negation"
evalUnOps Not (VBool bool) = Right $ VBool (not bool)
evalUnOps Not _            = Left "Type error: Incorrect type for logical NOT"


evalBinOps :: BinOp -> Value -> Value -> Either String Value
evalBinOps Add (VInt x) (VInt y) = Right $ VInt (x + y) 
evalBinOps Add (VString x) (VString y) = Right $ VString (x ++ y) 
evalBinOps Add _        _        = Left "Type error: incorrect types for Add"
evalBinOps Mul (VInt x) (VInt y) = Right $ VInt (x * y)
evalBinOps Sub (VInt x) (VInt y) = Right $ VInt (x - y)
evalBinOps Div (VInt x) (VInt y) 
    | y /= 0                     = Right $ VInt (x `div` y)
    | otherwise                  = Left "Runtime error: attempt to divide by 0"
evalBinOps Mod (VInt x) (VInt y) 
    | y /= 0                     = Right $ VInt (x `mod` y)
    | otherwise                  = Left "Runtime error: attempt to perform modulo division by 0"
evalBinOps Eq (VInt x) (VInt y)  = Right $ VBool $ x == y
evalBinOps Neq (VInt x) (VInt y)  = Right $ VBool $ x /= y
evalBinOps Lt (VInt x) (VInt y)  = Right $ VBool $ x < y
evalBinOps Lte (VInt x) (VInt y)  = Right $ VBool $ x <= y
evalBinOps Gt (VInt x) (VInt y)  = Right $ VBool $ x > y
evalBinOps Gte (VInt x) (VInt y)  = Right $ VBool $ x >= y
evalBinOps Eq (VBool x) (VBool y)  = Right $ VBool $ x == y
evalBinOps Neq (VBool x) (VBool y)  = Right $ VBool $ x /= y
evalBinOps Lt (VBool x) (VBool y)  = Right $ VBool $ x < y
evalBinOps Lte (VBool x) (VBool y)  = Right $ VBool $ x <= y
evalBinOps Gt (VBool x) (VBool y)  = Right $ VBool $ x > y
evalBinOps Gte (VBool x) (VBool y)  = Right $ VBool $ x >= y
evalBinOps Eq (VString x) (VString y)  = Right $ VBool $ x == y
evalBinOps Neq (VString x) (VString y)  = Right $ VBool $ x /= y
evalBinOps And (VBool x) (VBool y)  = Right $ VBool $ x && y
evalBinOps Or (VBool x) (VBool y)  = Right $ VBool $ x || y
evalBinOps op  _        _        = Left $ "Runtime error: attempt to apply " ++ show op ++ " with incorrect types"


--Typechecking
typecheck :: Variables -> Expr -> Either String Type
typecheck _ (Lit (VInt    _)) = Right TInt
typecheck _ (Lit (VBool   _)) = Right TBool
typecheck _ (Lit (VString _)) = Right TString

typecheck vars (Set name expr) = do
    let exprType = typecheck vars expr
    case exprType of
        Left err -> Left err
        Right exprType' -> case lookup name vars of 
            Just (existingType, _) | existingType /= exprType' -> 
                Left ("Type error in Set: Variable " ++ name ++ " has type " ++ show existingType ++ " but the expression evaluates to " ++ show exprType)
            _ -> Right TNone

typecheck vars (Get name) = do
    case lookup name vars of
        Nothing -> Left ("Variable " ++ name ++ " not found")
        Just (existingType, value) -> Right existingType

typecheck vars (BinOp op expr1 expr2) = do
    let exprType1 = typecheck vars expr1
    let exprType2 = typecheck vars expr2
    case (op, exprType1, exprType2) of
        (Add, Right TInt, Right TInt) -> Right TInt
        (Add, Right TString, Right TString) -> Right TString
        (Mul, Right TInt, Right TInt) -> Right TInt
        (Sub, Right TInt, Right TInt) -> Right TInt
        (Div, Right TInt, Right TInt) -> Right TInt
        (Mod, Right TInt, Right TInt) -> Right TInt
        (Eq, Right TInt, Right TInt)  -> Right TBool
        (Neq, Right TInt, Right TInt)  -> Right TBool
        (Lt, Right TInt, Right TInt)  -> Right TBool
        (Lte, Right TInt, Right TInt)  -> Right TBool
        (Gt, Right TInt, Right TInt)  -> Right TBool
        (Gte, Right TInt, Right TInt)  -> Right TBool
        (Eq, Right TBool, Right TBool)  -> Right TBool
        (Neq, Right TBool, Right TBool)  -> Right TBool
        (Lt, Right TBool, Right TBool)  -> Right TBool
        (Lte, Right TBool, Right TBool)  -> Right TBool
        (Gt, Right TBool, Right TBool)  -> Right TBool
        (Gte, Right TBool, Right TBool)  -> Right TBool
        (Eq, Right TString, Right TString)  -> Right TBool
        (Neq, Right TString, Right TString)  -> Right TBool
        (And, Right TBool, Right TBool)  -> Right TBool
        (Or, Right TBool, Right TBool)  -> Right TBool
        (_,  Left err1, Left err2) -> Left $ err1 ++ " and " ++ err2
        (_, Left err, _) -> Left err
        (_, _, Left err) -> Left err
        _                 -> Left ("Type error: " ++ show op ++ " cannot be applied to types " ++ show exprType1 ++ " and " ++ show exprType2)
    
typecheck vars (UnOp op expr) = do
    let exprType = typecheck vars expr
    case (op, exprType) of
        (Neg, Right TInt) -> Right TInt
        (Not, Right TBool) -> Right TBool
        _                 -> Left ("Type error: incorrect type for " ++ show op)

typecheck vars (IfElse pred expr1 expr2) = 
    checkThrough vars TNone [(pred, TBool, "if-else statement"), (expr1, TBool, "if block"), (expr2, TBool, "else block")]

typecheck vars (Seq []) = Right TNone
typecheck vars (Seq (expr:exprs)) = do
    checkThrough vars TNone [(expr, TNone, "sequence block")]
    typecheck vars (Seq exprs)

typecheck vars (While pred expr) = 
    checkThrough vars TNone [(pred, TBool, "while condition"), (expr, TNone, "while loop")]

typecheck vars (DoWhile pred expr) = 
    checkThrough vars TNone [(pred, TBool, "do-while condition"), (expr, TNone, "do-while loop")]

typecheck vars (Print expr) = do
    case typecheck vars expr of 
        Left error -> Left error
        Right _  -> Right TNone

typecheck vars x = do
  Left $ "Syntax error: unsupported " ++ show x ++ " operation" 

--Helper functions for typechecking
checkThrough :: Variables -> Type -> [(Expr, Type, String)] -> Either String Type
checkThrough _    x []                                 = Right x
checkThrough vars x ((expr, expectedType, failStr):es) = do
    case typecheck vars expr of
        Left str -> Left str
        Right testType | testType == expectedType -> do checkThrough vars x es
        Right x -> Left $ "Type error: incorrect type " ++ show x ++ " within " ++ failStr