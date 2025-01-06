module Evaluator (evaluate) where
import Prelude hiding (lookup)
import Data.Map ( Map, insert, lookup, empty )
import Definitions
import Typechecker


evaluate :: Variables -> Expr -> IO (Value, Variables)
evaluate vars (Lit value) = return (value, vars)

evaluate vars (Declare varType name expr) = do
  case lookup name vars of
    Just _ -> do
      putStrLn $ "Compile error: variable '" ++ name ++ "' is already assigned"
      return (VError, vars)
    Nothing -> do
      (val, _) <- evaluate vars expr
      if val == VError
      then return (VError, vars)
      else return (VNone, insert name (varType, val) vars)

evaluate vars (Set name expr) = 
  case lookup name vars of
    Just (existingType, _) -> do
      (val, _) <- evaluate vars expr
      if val == VError
      then return (VError, vars)    
      else return (VNone, insert name (existingType, val) vars)
    _ -> do
      putStrLn $ "Compile error: variable '" ++ name ++ "' not declared"
      return (VError, vars)

evaluate vars (Get name) = case lookup name vars of
    Nothing -> do
        putStrLn $ "Compile error: variable " ++ name ++ " not defined within the current scope"
        return (VError, vars)
    Just (_, val) -> return (val, vars)

evaluate vars (BinOp op expr1 expr2) = do
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

evaluate vars (UnOp op expr) = do
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

evaluate vars (IfElse pred trueExpr falseExpr) = do
  (predicateValue, vars') <- evaluate vars pred
  if predicateValue == VError 
  then return (VError, vars')
  else case predicateValue of 
    VBool True -> evaluate vars' trueExpr
    VBool False -> evaluate vars' falseExpr

evaluate vars (Seq [])         = return (VNone, vars)
evaluate vars (Seq (expr:exprs)) = do
  (value, vars') <- evaluate vars expr
  case value of 
    VNone -> evaluate vars' (Seq exprs)
    _     -> do
      putStrLn "Compiler error: unexpected value in sequence"
      return (VError, vars')

evaluate vars (While pred expr) = do
  (predicateValue, vars') <- evaluate vars pred
  case predicateValue of 
    VError      -> return (VError, vars')
    VBool False -> return (VNone, vars')
    _           -> do
      (_, vars'') <- evaluate vars' expr
      evaluate vars'' (While pred expr)
    
evaluate vars (DoWhile pred expr) = do
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

evaluate vars (Print expr) = do
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
evalBinOps Eq  (VInt x) (VInt y)  = Right $ VBool $ x == y
evalBinOps Neq (VInt x) (VInt y)  = Right $ VBool $ x /= y
evalBinOps Lt  (VInt x) (VInt y)  = Right $ VBool $ x < y
evalBinOps Lte (VInt x) (VInt y)  = Right $ VBool $ x <= y
evalBinOps Gt  (VInt x) (VInt y)  = Right $ VBool $ x > y
evalBinOps Gte (VInt x) (VInt y)  = Right $ VBool $ x >= y
evalBinOps Eq  (VBool x) (VBool y)  = Right $ VBool $ x == y
evalBinOps Neq (VBool x) (VBool y)  = Right $ VBool $ x /= y
evalBinOps Lt  (VBool x) (VBool y)  = Right $ VBool $ x < y
evalBinOps Lte (VBool x) (VBool y)  = Right $ VBool $ x <= y
evalBinOps Gt  (VBool x) (VBool y)  = Right $ VBool $ x > y
evalBinOps Gte (VBool x) (VBool y)  = Right $ VBool $ x >= y
evalBinOps Eq  (VString x) (VString y)  = Right $ VBool $ x == y
evalBinOps Neq (VString x) (VString y)  = Right $ VBool $ x /= y
evalBinOps And (VBool x) (VBool y)  = Right $ VBool $ x && y
evalBinOps Or  (VBool x) (VBool y)  = Right $ VBool $ x || y
evalBinOps op  _        _        = Left $ "Type error: attempt to apply " ++ show op ++ " with incorrect types"