module Evaluator (evaluate) where
import Prelude hiding (lookup)
import Data.Map ( Map, insert, lookup, empty, union, fromList, toList, delete )
import Definitions
import Utility
import qualified Data.Bifunctor

--Evaluation function for the interpreter - the evaluation function assumes typechecking has already taken place
--Environment - the 'memory' assigned to our program, storing the variables and functions
--Expression - our program
--(Value, Environment) - the value represents the 'value' of our expression
--                       the environment represents the changed 'memory' following the execution of the expression
evaluate :: Environment -> Expr -> IO (Value, Environment)
evaluate env (Lit value) = return (value, env) -- Calling a literal

evaluate env (Declare varType name expr) = do
  let (vars, funcs) = env
  case lookupVariable name vars of                                                                --Looking up a variable in the scoped environment     --SHOULD ONLY LOOK AT THE MOST RECENT SCOPE
    Just _ -> do                                                                                  --The variable already exists - we cannot declare a variable with the same name
      putStrLn $ "Compile error: variable '" ++ name ++ "' is already assigned"
      return (VError, env)
    Nothing -> do                                                                                 --The variable doesn't exists - we can declare a new variable
      (val, _) <- evaluate env expr                                                               --Evaluate the expression for the declaration
      if val == VError
      then return (VError, env)                                                                   --Error in the expression
      else return (VNone, (insertVariable name (varType, val) vars, funcs))                       --Add a new variable to the current scope

evaluate env (Set name expr) =
  let (vars, funcs) = env
  in case lookupVariable name vars of                                                             --Looking up a variable in the scoped environment  
    Just (existingType, _) -> do                                                                  --The variable exists
      (val, env') <- evaluate env expr                                                            --Evaluate the expression for the assignment
      let (vars', funcs') = env'
      if val == VError
      then return (VError, env')                                                                  --Error in the expression
      else return (VNone, (updateVariable name (existingType, val) vars', funcs'))                --Updates a variable
    _ -> do
      putStrLn $ "Compile error: variable '" ++ name ++ "' not declared"
      return (VError, env)

evaluate env (Get name) =
  let (vars, funcs) = env
  in case lookupVariable name vars of                                                                   --Looking up a variable in the scoped environment
    Nothing -> do                                                                                       --Variable doesn't exist
        putStrLn $ "Compile error: variable " ++ name ++ " not defined within the current scope"
        return (VError, env)
    Just (_, val) -> return (val, env)                                                                  --Returns the value of the variable

evaluate env (BinOp op expr1 expr2) = do
  (value1, env') <- evaluate env expr1
  (value2, env'') <- evaluate env' expr2                                                                           --Evaluate the two operands
  if value1 == VError || value2 == VError
  then do
    putStrLn $ "Compile error: evaluation error on operands " ++ show value1 ++ " and " ++ show value2
    return (VError, env'')                                                                                         --Errors with one of the operands
  else do
    let result = evalBinOps op value1 value2                                                                       --Evaluate the binary operation with the two operands
    case result of
      Left str -> do                                                                                               --Errors with evaluation of the binary operation
          putStrLn str
          return (VError, env'')
      Right validResult -> return (validResult, env'')                                                             --Valid operation

evaluate env (UnOp op expr) = do
  (value, env') <- evaluate env expr                                                                               --Evaluates the operand
  if value == VError
  then do
    putStrLn $ "Compile error: evaluation error on operand " ++ show value
    return (VError, env')                                                                                          --Errors with the operand
  else do
    let result = evalUnOps op value                                                                                --Evaluate the unary operation with the operand
    case result of
      Left str -> do
        putStrLn str
        return (VError, env')
      Right validResult -> return (validResult, env')                                                              --Valid operation

evaluate env (IfElse pred trueExpr falseExpr) = do
  (predicateValue, env') <- evaluate env pred                                                                     --Evaluates the predicate
  if predicateValue == VError
  then return (VError, env')
  else case predicateValue of
    VBool True -> evaluate env' trueExpr
    VBool False -> evaluate env' falseExpr
{-
evaluate env (Seq [])         = return (VNone, env)
evaluate env (Seq (expr:exprs)) = do
  (value, env') <- evaluate env expr
  case value of 
    VNone -> evaluate env' (Seq exprs)
    _     -> do
      --putStrLn "Compiler error: unexpected value in sequence"
      return (VError, env')
-}

evaluate env (Seq []) = return (VNone, env)
evaluate env@(vars, funcs) (Seq exprs) = do
  let scopedEnv = (enterScope vars, funcs)
  --print scopedEnv
  scopedResult <- evaluateSequence scopedEnv exprs
  case scopedResult of
    (value, scopedEnv') -> do
      let finalEnv = Data.Bifunctor.first exitScope scopedEnv'
      --print (fst finalEnv)
      return (value, finalEnv)
  where
    evaluateSequence env [] = return (VNone, env)
    evaluateSequence env (expr:exprs) = do
      (value, env') <- evaluate env expr
      --print value
      case value of
        VError     -> do
          putStrLn "Compile error: error within sequence block"
          return (VError, env')
        VNone      -> evaluateSequence env' exprs
        _ -> return (value, env')


evaluate env (While pred expr) = do
  (predicateValue, env') <- evaluate env pred
  case predicateValue of
    VError      -> return (VError, env')
    VBool False -> return (VNone, env')
    _           -> do
      (_, env'') <- evaluate env' expr
      evaluate env'' (While pred expr)

evaluate env (DoWhile pred expr) = do
  (value, env') <- evaluate env expr
  if value /= VNone
  then return (VError, env')
  else do
    (predicateValue, env'') <- evaluate env' pred
    case predicateValue of
      VError      -> return (VError, env')
      VBool False -> return (VNone, env'')
      _ -> evaluate env'' (DoWhile pred expr)

evaluate env Skip = return (VNone, env)

evaluate env (Print expr) = do
  (value, env') <- evaluate env expr
  case value of
    VError -> return (VError, env')
    _      -> do
      putStrLn (prettyPrint value)
      return (VNone, env')

evaluate env (Return expr) = evaluate env expr

evaluate env (Function name params returnType expr) =
  let func = (params, returnType, expr)
      (vars, funcs) = env
  in return (VNone, (vars, insert name func funcs))

evaluate (vars, funcs) (Procedure name params expr) =
  let func = (params, TNone, expr)
  in return (VNone, (vars, insert name func funcs))

evaluate env@(_, funcs) (Call name args) =
  case lookup name funcs of
    Nothing -> do
      putStrLn $ "Compile error: function '" ++ name ++ "' is not defined"
      return (VError, env)
    Just (params, retType, body) ->
      if length params /= length args
      then do
        putStrLn $ "Compile error: incorrect number of arguments supplied to function '" ++ name ++ "'"
        return (VError, env)
      else do
        (definedArgs, env') <- evalArgs env args
        let (varScopes, funcs') = env'

        let boundParams = fromList [(n, (t, v)) | ((t, n), v) <- zip params definedArgs]
        let newVarScopes = enterScope varScopes
        let funcScope = insertVariables (toList boundParams) newVarScopes

        let funcEnv = (funcScope, funcs')
        (result, updatedEnv) <- evaluate funcEnv body
        let (updatedScopes, _) = updatedEnv
        let finalScopes = exitScope updatedScopes
        let newEnv = (finalScopes, funcs')

        case retType of
          TNone -> return (VNone, newEnv)
          _     -> return (result, newEnv)


--Helper for inserting variables into a new scope
insertVariables :: [(String, (Type, Value))] -> ScopedVariables -> ScopedVariables
insertVariables vars scopes = foldr (\(name, typeVal) sc -> insertVariable name typeVal sc) scopes vars


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


evalArgs :: Environment -> [Expr] -> IO ([Value], Environment)
evalArgs env []     = return ([], env)
evalArgs env (expr:exprs) = do
  (val, env') <- evaluate env expr
  (vals, env'') <- evalArgs env' exprs
  return (val : vals, env'')