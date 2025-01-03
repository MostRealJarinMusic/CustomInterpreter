module Typechecker (typecheck) where
import Prelude hiding (lookup)
import Data.Map ( Map, lookup )
import Definitions

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