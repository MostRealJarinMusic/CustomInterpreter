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
testProgram = Seq (Set "x" (Lit (VInt 7))) (Seq (Set "y" (Lit (VInt 6))) (Set "z" (BinOp Add (Get "x") (Get "y"))))

evaluateProgram :: Expr -> IO (Value, Variables)
evaluateProgram = evaluate empty 

evaluate :: Variables -> Expr -> IO (Value, Variables)
evaluate vars (Lit value) = return (value, vars)

evaluate vars (Set name expr) = do
    let existingVar = lookup name vars
    let checkedType = typecheck vars expr
    case (existingVar, checkedType) of
        (Just (existingType, _), Right exprType) | existingType /= exprType -> 
            error ("Type mismatch for " ++ name)
        (_, Left str) -> error str
        (_, Right exprType) -> do
            (val, _) <- evaluate vars expr
            return (val, insert name (exprType, val) vars)


evaluate vars (Get name) = case lookup name vars of
    Nothing -> error ("No variable called " ++ name)
    Just (_, val) -> return (val, vars)

evaluate vars (BinOp op expr1 expr2) = checkThenEval vars (BinOp op expr1 expr2) evaluate'
    where  
        evaluate' = do
            (value1, vars') <- evaluate vars expr1
            (value2, vars'') <- evaluate vars' expr2
            return (evalBinOps op value1 value2, vars'')

evaluate vars (UnOp op expr) = checkThenEval vars (UnOp op expr) evaluate'
    where
        evaluate' = do
            (value, vars') <- evaluate vars expr
            return (evalUnOps op value, vars')

evaluate vars (IfElse pred trueExpr falseExpr) = 
    checkThenEval vars (IfElse pred trueExpr falseExpr) evaluate'
    where
        evaluate' = do
            (predicateValue, vars') <- evaluate vars pred
            case predicateValue of 
                VBool True -> evaluate vars' trueExpr
                VBool False -> evaluate vars' falseExpr

evaluate vars (Seq expr1 expr2) = do
    (_, vars') <- evaluate vars expr1
    evaluate vars' expr2

evaluate vars (While pred expr) = checkThenEval vars (While pred expr) evaluate'
    where 
        evaluate' = do
            (predicateValue, vars') <- evaluate vars pred
            case predicateValue of 
                VBool False -> return (VNone, vars')
                _ -> do
                    (_, vars'') <- evaluate vars' expr
                    evaluate vars'' (While pred expr)
    
evaluate vars (DoWhile pred expr) = checkThenEval vars (DoWhile pred expr) evaluate'
    where
        evaluate' = do
            (_, vars') <- evaluate vars expr
            (predicateValue, vars'') <- evaluate vars' pred
            case predicateValue of 
                VBool False -> return (VNone, vars'')
                _ -> evaluate vars'' (DoWhile pred expr)

evaluate vars Skip = return (VNone, vars)


--Helper for typechecking
checkThenEval :: Variables -> Expr -> IO (Value, Variables) -> IO (Value, Variables)
checkThenEval vars testExpr evalFun = do
    let checkedType = typecheck vars testExpr
    case checkedType of
        Left str -> error str
        Right _ -> evalFun 


--Helper functions for evaluation
evalUnOps :: UnOp -> Value -> Value
evalUnOps Neg (VInt x) = VInt (negate x)


evalBinOps :: BinOp -> Value -> Value -> Value
evalBinOps Add (VInt x) (VInt y) = VInt (x + y)


typecheck :: Variables -> Expr -> Either String Type
typecheck _ (Lit (VInt    _)) = Right TInt
typecheck _ (Lit (VBool   _)) = Right TBool
typecheck _ (Lit (VString _)) = Right TString

typecheck vars (Set name expr) = do
    exprType <- typecheck vars expr
    case lookup name vars of 
        Just (existingType, _) | existingType /= exprType -> 
            Left ("Type error for " ++ name)
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
        _                 -> Left ("Incorrect types for function" ++ show op)
typecheck vars (UnOp op expr) = do
    exprType <- typecheck vars expr
    case (op, exprType) of
        (Neg, TInt) -> Right TInt
        _                 -> Left ("Incorrect type for function" ++ show op)
typecheck vars (IfElse predicate expr1 expr2) = do
    predicateType <- typecheck vars predicate
    unless (predicateType == TBool) (Left "Incorrect type for if-else condition")
    exprType1 <- typecheck vars expr1
    exprType2 <- typecheck vars expr2
    if exprType1 == exprType2 
    then return exprType1
    else Left "Block types for if-else statement do not match"

