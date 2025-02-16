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
  then return (VError, env')                                                                                      --Errors with the predicate
  else case predicateValue of
    VBool True -> evaluate env' trueExpr                                                                          --Evaluate true branch
    VBool False -> evaluate env' falseExpr                                                                        --Evaluate false branch
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

evaluate env (Seq []) = return (VNone, env)                                                                       --End of a sequence block
evaluate env@(vars, funcs) (Seq exprs) = do
  let scopedEnv = (enterScope vars, funcs)                                                                        --Enter a new variable scope within a sequence block
  --print scopedEnv
  scopedResult <- evaluateSequence scopedEnv exprs                                                                --Evaluate the sequence block
  case scopedResult of
    (value, scopedEnv') -> do
      let finalEnv = Data.Bifunctor.first exitScope scopedEnv'                                                    --Exit the new variable scope
      --print (fst finalEnv)
      return (value, finalEnv)                                                                                    
  where
    evaluateSequence env [] = return (VNone, env)                                                                 --End of a sequence block
    evaluateSequence env (expr:exprs) = do
      (value, env') <- evaluate env expr                                                                          --Evaluate a line in the sequence
      --print value
      case value of
        VError     -> do
          putStrLn "Compile error: error within sequence block"
          return (VError, env')                                                                                   --Error on that line in the sequence
        VNone      -> evaluateSequence env' exprs                                                                 --Recursively evaluate the next section in the sequence
        _ -> return (value, env')                                                                                 --If we are expected to return something in the block, return it


evaluate env (While pred expr) = do
  (predicateValue, env') <- evaluate env pred                                                                     --Evaluate the predicate
  case predicateValue of
    VError      -> return (VError, env')                                                                          --Error with the predicate
    VBool False -> return (VNone, env')                                                                           --Predicate evaluates to false - exit the while loop
    _           -> do
      (_, env'') <- evaluate env' expr                                                                            --Evaluate the while block
      evaluate env'' (While pred expr)                                                                            --Continue the while loop

evaluate env (DoWhile pred expr) = do   
  (value, env') <- evaluate env expr                                                                              --Evaluate the predicate
  if value /= VNone
  then return (VError, env')                                                                                      --Error with the predicate
  else do
    (predicateValue, env'') <- evaluate env' pred                                                                 --Evaluate the do-while block
    case predicateValue of
      VError      -> return (VError, env')                                                                        --Error with the predicate
      VBool False -> return (VNone, env'')                                                                        --Predicate evaluates to false - exit the do-while loop
      _ -> evaluate env'' (DoWhile pred expr)                                                                     --Continue the do-while block

evaluate env Skip = return (VNone, env)                                                                           --Skip

evaluate env (Print expr) = do
  (value, env') <- evaluate env expr                                                                              --Evaluate the expression
  case value of
    VError -> return (VError, env')                                                                               --Error with the expression
    _      -> do
      putStrLn (prettyPrint value)                                                                                --Print to the console
      return (VNone, env')                                

evaluate env (Return expr) = evaluate env expr                                                                    --Evaluate the expression

evaluate (vars, funcs) (Function name params returnType expr) =
  let func = (params, returnType, expr)                                                                           --Define the function
  in return (VNone, (vars, insert name func funcs))                                                               --Add the function to the environment

evaluate (vars, funcs) (Procedure name params expr) =
  let func = (params, TNone, expr)                                                                                --Define the procedure
  in return (VNone, (vars, insert name func funcs))                                                               --Add the procedure to the environment

evaluate env@(_, funcs) (Call name args) =
  case lookup name funcs of                                                                                       --Look up the function in the environment
    Nothing -> do
      putStrLn $ "Compile error: function '" ++ name ++ "' is not defined"                                        --The function does not exist
      return (VError, env)
    Just (params, retType, body) ->                                                                               --The function exists
      if length params /= length args                                                                             --Matching number of arguments - assuming they are the correct corresponding types
      then do
        putStrLn $ "Compile error: incorrect number of arguments supplied to function '" ++ name ++ "'"           --Incorrect number of arguments
        return (VError, env)
      else do
        (definedArgs, env') <- evalArgs env args                                                                  --Evaluate all of the arguments
        let (varScopes, funcs') = env'

        let boundParams = fromList [(n, (t, v)) | ((t, n), v) <- zip params definedArgs]                          --Create a new set of variables from the entered parameters
        let newVarScopes = enterScope varScopes                                                                   --Enter a new variable scope
        let funcScope = insertVariables (toList boundParams) newVarScopes                                         --Insert the variables into the new scope

        let funcEnv = (funcScope, funcs')                                                                         --Alias for the new function environment
        (result, updatedEnv) <- evaluate funcEnv body                                                             --Evaluate the function - works for both functions and procedures
        let (updatedScopes, _) = updatedEnv                                                                       --Alias for the updated environment
        let finalScopes = exitScope updatedScopes                                                                 --Exit the scope
        let newEnv = (finalScopes, funcs')                                                                        --Alias for the new scope

        case retType of                                                                                           --Return for the function
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
--Defined 'base' unary operators
evalUnOps :: UnOp -> Value -> Either String Value
evalUnOps Neg (VInt x) = Right $ VInt (negate x)
evalUnOps Neg _        = Left "Type error: incorrect type for negation"
evalUnOps Not (VBool bool) = Right $ VBool (not bool)
evalUnOps Not _            = Left "Type error: Incorrect type for logical NOT"

--Defined 'base' binary operators
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
evalArgs env []     = return ([], env)                                  --No arguments left to evaluate - return the evaluted arguments
evalArgs env (expr:exprs) = do                                          
  (val, env') <- evaluate env expr                                      --Evaluate the current argument
  (vals, env'') <- evalArgs env' exprs                                  --Recursively evaluate the rest
  return (val : vals, env'')                                            --Accumulate the evaluated arguments and return them