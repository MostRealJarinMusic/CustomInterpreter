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
testProgram = Seq (Set "x" (Lit (VInt 7))) (Seq (Set "y" (Lit (VString "6"))) (Set "z" (BinOp Add (Get "x") (Get "y"))))
--testProgram = Set "x" (BinOp Add (Lit (VInt 7)) (Lit (VString "6")))
--testProgram = Seq (Set "x" (Lit (VInt 42))) (Print (Get "x"))

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

evaluate vars (Seq expr1 expr2) = checkThenEval vars expr1 evaluate'
    where 
        evaluate' = do
            (value, vars') <- evaluate vars expr1
            case value of 
                VNone -> evaluate vars' expr2
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
evalBinOps Add _        _        = Left "Type error: incorrect types for Add"
evalBinOps Mul (VInt x) (VInt y) = Right $ VInt (x * y)
evalBinOps Sub (VInt x) (VInt y) = Right $ VInt (x - y)
evalBinOps Div (VInt x) (VInt y) 
    | y /= 0                     = Right $ VInt (x `div` y)
    | otherwise                  = Left "Runtime error: attempt to divide by 0"
evalBinOps Mod (VInt x) (VInt y) 
    | y /= 0                     = Right $ VInt (x `mod` y)
    | otherwise                  = Left "Runtime error: attempt to perform modulo division by 0"
evalBinOps op  _        _        = Left $ "Runtime error: attempt to apply " ++ show op ++ " with incorrect types"


--Typechecking
typecheck :: Variables -> Expr -> Either String Type
typecheck _ (Lit (VInt    _)) = Right TInt
typecheck _ (Lit (VBool   _)) = Right TBool
typecheck _ (Lit (VString _)) = Right TString

typecheck vars (Set name expr) = do
    exprType <- typecheck vars expr
    case lookup name vars of 
        Just (existingType, _) | existingType /= exprType -> 
            Left ("Type error in Set: Variable " ++ name ++ " has type " ++ show existingType ++ " but the expression evaluates to " ++ show exprType)
        _ -> Right exprType

typecheck vars (Get name) = do
    case lookup name vars of
        Nothing -> Left ("Variable " ++ name ++ " not found")
        Just (existingType, value) -> Right existingType

typecheck vars (BinOp op expr1 expr2) = do
    exprType1 <- typecheck vars expr1
    exprType2 <- typecheck vars expr2
    case (op, exprType1, exprType2) of
        (Add, TInt, TInt) -> Right TInt
        (Mul, TInt, TInt) -> Right TInt
        (Sub, TInt, TInt) -> Right TInt
        (Div, TInt, TInt) -> Right TInt
        (Mod, TInt, TInt) -> Right TInt
        _                 -> Left (show op ++ " cannot be applied to types " ++ show exprType1 ++ " and " ++ show exprType2)
    
typecheck vars (UnOp op expr) = do
    exprType <- typecheck vars expr
    case (op, exprType) of
        (Neg, TInt) -> Right TInt
        _                 -> Left ("Incorrect type for function " ++ show op)

typecheck vars (IfElse pred expr1 expr2) = do
    predicateType <- typecheck vars pred
    unless (predicateType == TBool) (Left "Incorrect type for if-else condition")
    exprType1 <- typecheck vars expr1
    exprType2 <- typecheck vars expr2
    if exprType1 == exprType2 
    then return exprType1
    else Left "Block types for if-else statement do not match"

typecheck vars (While pred expr) = do
    predType <- typecheck vars pred
    unless (predType == TBool) (Left "Incorrect type for while condition")
    exprType <- typecheck vars expr
    unless (exprType == TNone) (Left "Incorrect type within while loop")
    Right TNone

typecheck vars (DoWhile pred expr) = do
    predType <- typecheck vars pred
    unless (predType == TBool) (Left "Incorrect type for do-while condition")
    exprType <- typecheck vars expr
    unless (exprType == TNone) (Left "Incorrect type within do-while loop")
    Right TNone

typecheck vars (Print expr) = do
    case typecheck vars expr of 
        Left error -> Left error
        Right _  -> Right TNone
