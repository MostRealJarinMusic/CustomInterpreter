import Prelude hiding (lookup)
import Control.Monad ( unless )
import Data.Map ( Map, insert, lookup, empty )

data Expr = 
      Set String Expr
    | Get String
    | Lit Value
    | BinOp BinOp Expr Expr
    | UnOp UnOp Expr
    | IfElse Expr Expr Expr
    | Seq Expr Expr
    | While Expr Expr
    | DoWhile Expr Expr
    | Skip
    | Lambda Expr Expr
    | Print Expr
    deriving (Show)

data BinOp = 
      Add | Mul | Sub | Div | Mod 
    | Eq | Neq | Lt | Lte | Gt | Gte 
    | And | Or  
    deriving (Show)

data UnOp = 
      Neg
    | Not
    deriving (Show)

data Value = 
      VInt Int
    | VString String
    | VBool Bool
    | VNone
    deriving (Eq, Show)

data Type = 
      TInt
    | TString
    | TBool 
    | TBlock
    deriving (Eq, Show)

type Variables = Map String (Type, Value)


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

evaluate vars (BinOp operator expr1 expr2) = do
    let checkedType = typecheck vars (BinOp operator expr1 expr2)
    case checkedType of 
        Left str -> error str
        Right _ -> do
            (value1, vars') <- evaluate vars expr1
            (value2, vars'') <- evaluate vars' expr2
            return (evalBinOps operator value1 value2, vars'')

evaluate vars (UnOp operator expr) = do
    let checkedType = typecheck vars (UnOp operator expr)
    case checkedType of 
        Left str -> error str
        Right _ -> do
            (value, vars') <- evaluate vars expr
            return (evalUnOps operator value, vars')

evaluate vars (IfElse predicate trueExpr falseExpr) = do
    let checkedType = typecheck vars (IfElse predicate trueExpr falseExpr)
    case checkedType of 
        Left str -> error str
        Right _ -> do
            (predicateValue, vars') <- evaluate vars predicate
            case predicateValue of 
                VBool True -> evaluate vars' trueExpr
                VBool False -> evaluate vars' falseExpr

evaluate vars (Seq expr1 expr2) = do
    (_, vars') <- evaluate vars expr1
    evaluate vars' expr2

evaluate vars (While predicate expr) = do
    let checkedType = typecheck vars (While predicate expr)
    case checkedType of 
        Left str -> error str
        Right _ -> do 
            (predicateValue, vars') <- evaluate vars predicate
            case predicateValue of 
                VBool False -> return (VNone, vars')
                _ -> do
                    (_, vars'') <- evaluate vars' expr
                    evaluate vars'' (While predicate expr)

evaluate vars (DoWhile predicate expr) = do
    let checkedType = typecheck vars (DoWhile predicate expr)
    case checkedType of
        Left str -> error str
        Right _ -> do 
            (_, vars') <- evaluate vars expr
            (predicateValue, vars'') <- evaluate vars' predicate
            case predicateValue of 
                VBool False -> return (VNone, vars'')
                _ -> evaluate vars'' (DoWhile predicate expr)


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

