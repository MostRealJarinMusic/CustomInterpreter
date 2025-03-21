module Evaluator (evaluate) where
import Prelude hiding (lookup)
import Data.Map ( Map, insert, lookup, empty, union, fromList, toList, delete )
import Definitions
import Utility
import qualified Data.Bifunctor

--Evaluation function for the interpreter - the evaluation function assumes typechecking has already taken place
--Environment - the 'memory' assigned to our program, storing the variables and functions
--Expression - our program
--(Environment, Value) - the value represents the 'value' of our expression
--                       the environment represents the changed 'memory' following the execution of the expression


evaluate :: [Stmt] -> IO ()
evaluate program = do
  evalBlock ([empty], empty) program
  return ()


evalExpr :: Environment -> Expr -> IO (Environment, Value)
evalExpr env (Lit value) = return (env, value)

evalExpr env@(scopedVars, _) (Get name) =
  case lookupVariable name scopedVars of
    Nothing -> do
      putStrLn $ "Compile error: variable " ++ name ++ " not defined within the current scope"
      return (env, VError)
    Just (_, val) -> return (env, val)

evalExpr env (BinOp op expr1 expr2) = do
  (env', value1) <- evalExpr env expr1
  (env'', value2) <- evalExpr env' expr2
  if value1 == VError || value2 == VError
  then do
    putStrLn $ "Compile error: evaluation error on operands " ++ show value1 ++ " and " ++ show value2
    return (env, VError)
  else do
    let result = evalBinOps op value1 value2
    case result of
      Left str -> do
          putStrLn str
          return (env'', VError)
      Right validResult -> return (env'', validResult)

evalExpr env (UnOp op expr) = do
  (env', value) <- evalExpr env expr
  case value of
    VError -> do
      putStrLn $ "Compile error: evaluation error on operand " ++ show value
      return (env, VError)
    _ -> do
      case evalUnOps op value of
        Left str -> do
          putStrLn str
          return (env', VError)
        Right validResult -> return (env', validResult)


evalExpr env@(scopedVars, funcs) (Call name args) = do
  case lookup name funcs of
    Just (params, retType, body) ->
      if length params /= length args
      then do
        putStrLn $ "Compile error: incorrect number of arguments supplied to function '" ++ name ++ "'"
        return (env, VError)
      else do
        (env', definedArgs) <- evalArgs env args
        if VError `elem` definedArgs
        then do
          putStrLn $ "Compile error: invalid arguments supplied to function '" ++ name ++ "'"
          return (env, VError)
        else do
          let paramBindings = zipWith (\(t, n) v -> (n, (t, v))) params definedArgs
          let newScope = fromList paramBindings
          let env'' = (newScope : scopedVars, funcs)
          ((scopedVars', funcs), val) <- evalBlock env'' body
          let env''' = (exitScope scopedVars', funcs) 
          return (env''', val)


evalStmt :: Environment -> Stmt -> IO (Environment, Value)
evalStmt env (ExprStmt expr) = do
  (env', result) <- evalExpr env expr
  return (env', result)

evalStmt env@(scopedVars, funcs) (Declare varType name expr) = do
  case lookup name (head scopedVars) of
    Just _ -> do
      putStrLn $ "Compile error: variable '" ++ name ++ "' is already assigned"
      return (env, VError)
    Nothing -> do
      ((scopedVars', funcs), val) <- evalExpr env expr
      if val == VError
      then return (env, VError)
      else return ((insertVariable name (varType, val) scopedVars', funcs), VNone)

evalStmt env@(scopedVars, funcs) (Set name expr) = do
  case lookupVariable name scopedVars of
    Just (existingType, _) -> do
      ((scopedVars', funcs), val) <- evalExpr env expr
      if val == VError
      then return (env, VError)
      else return ((updateVariable name (existingType, val) scopedVars', funcs), VNone)
    _ -> do
      putStrLn $ "Compile error: variable '" ++ name ++ "' not declared"
      return (env, VError)

evalStmt env (IfElse pred trueBranch maybeFalseBranch) = do
  (env', predVal) <- evalExpr env pred
  case predVal of
    VError -> return (env, VError)
    VBool True -> do
      evalStmt env' (Block trueBranch)
    VBool False -> case maybeFalseBranch of
      Just falseBranch -> evalStmt env' (Block falseBranch)
      Nothing -> return (env', VNone)

evalStmt env (While pred body) = do
  (env', predVal) <- evalExpr env pred
  case predVal of
    VError -> return (env, VError)
    VBool True -> do
      (env', value) <- evalStmt env (Block body)
      case value of
        VError -> return (env', VError)
        _ -> evalStmt env' (While pred body) 
    VBool False -> return (env, VNone)

evalStmt env (DoWhile pred body) = do
  (env', result) <- evalStmt env (Block body)
  case result of
    VError -> return (env', VError)
    _ -> evalStmt env' (While pred body)

evalStmt env (Return maybeExpr) = do
  case maybeExpr of
    Nothing -> return (env, VNone)
    Just expr -> do
      (env', val) <- evalExpr env expr
      return (env', val)
  
evalStmt env (Print expr) = do
  (env', val) <- evalExpr env expr
  case val of
    VError -> return (env, VError)
    _      -> do
      putStrLn (prettyPrint val)
      return (env', VNone)   

evalStmt env@(scopedVars, funcs) (Block body) = do
  let env' = (enterScope scopedVars, funcs)
  ((scopedVars', funcs), val) <- evalBlock env' body
  return ((exitScope scopedVars', funcs), val)

evalStmt env@(scopedVars, funcs) (Function name params retType body) = do
  let newFuncs = insert name (params, retType, body) funcs
  return ((scopedVars, newFuncs), VNone)

evalStmt env@(scopedVars, funcs) (Procedure name params body) = do
  let newFuncs = insert name (params, TNone, body) funcs
  return ((scopedVars, newFuncs), VNone)


evalBlock :: Environment -> [Stmt] -> IO (Environment, Value)
evalBlock env [] = return (env, VNone)
evalBlock env (stmt : rest) = do
  (env', val) <- evalStmt env stmt
  case val of
    VError -> return (env', VError)
    VNone -> evalBlock env' rest
    _ -> return (env', val)




{-
evaluate :: Environment -> Expr -> IO (Value, Environment)
evaluate env (Lit value) = return (value, env)                                                    --Calling a literal

evaluate env (Declare varType name expr) = do
  let (vars, funcs) = env
  case lookup name (head vars) of                                                                 --Looking up a variable in the most recent scope
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
    _ -> do                                                                                       --The variable does not exist
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

--Sequences do not enter a new scope
evaluate env (Seq [])           = return (VNone, env)                                                             --End of a sequence
evaluate env (Seq (expr:exprs)) = do
  (value, env') <- evaluate env expr                                                                              --Evaluate a line in the sequence
  --print value
  case value of
    VError     -> do
      putStrLn "Compile error: error within sequence block"
      return (VError, env')                                                                                       --Error on that line in the sequence
    VNone      -> evaluate env' (Seq exprs)                                                                       --Recursively evaluate the next section in the sequence
    _ -> return (value, env')                                                                                     --If we are expected to return something in the block, return it

--Blocks do enter a new scope
evaluate env               (Block [])    = return (VNone, env)                                                    --Empty scoped block
evaluate env@(vars, funcs) (Block exprs) = do
  let scopedEnv = (enterScope vars, funcs)                                                                        --Enter a new variable scope within the block
  scopedResult <- evaluateBlock scopedEnv exprs                                                                   --Evaluate the block
  case scopedResult of 
    (value, scopedEnv') -> do
      let finalEnv = Data.Bifunctor.first exitScope scopedEnv'                                                    --Exit the new variable scope and return
      return (value, finalEnv)
  where
    evaluateBlock env []           = return (VNone, env)                                                          --End of a block
    evaluateBlock env (expr:exprs) = do
      (value, env') <- evaluate env expr                                                                          --Evaluate a line in the block
      case value of 
        VError    -> do
          putStrLn "Compile error: error within scoped block"
          return (VError, env')                                                                                   --Error in the block    
        VNone     -> evaluateBlock env' exprs                                                                     --Recursively evaluate the next section in the sequence
        _         -> return (value, env')                                                                         --If we are expected to return something from the block, return it


evaluate env (IfElse pred trueExpr falseExpr) = do
  (predicateValue, env') <- evaluate env pred                                                                     --Evaluates the predicate
  if predicateValue == VError
  then return (VError, env')                                                                                      --Errors with the predicate
  else case predicateValue of
    VBool True -> evaluate env' trueExpr                                                                          --Evaluate true branch
    VBool False -> evaluate env' falseExpr                                                                        --Evaluate false branch

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
        let funcScopes = insertVariables (toList boundParams) newVarScopes                                         --Insert the variables into the new scope

        let funcEnv = (funcScopes, funcs')                                                                         --Alias for the new function environment
        print funcScopes
        (result, updatedEnv) <- evaluate funcEnv body                                                             --Evaluate the function - works for both functions and procedures
        let (updatedScopes, _) = updatedEnv                                                                       --Alias for the updated environment
        let finalScopes = exitScope updatedScopes                                                                 --Exit the scope
        let newEnv = (finalScopes, funcs')                                                                        --Alias for the new environment
        print finalScopes

        case retType of                                                                                           --Return for the function
          TNone -> return (VNone, newEnv)
          _     -> return (result, newEnv)


-}


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


evalArgs :: Environment -> [Expr] -> IO (Environment, [Value])
evalArgs env []     = return (env, [])                                  --No arguments left to evaluate - return the evaluted arguments
evalArgs env (expr:exprs) = do
  (env', val) <- evalExpr env expr                                              --Evaluate the current argument
  (env'', vals) <- evalArgs env' exprs                                  --Recursively evaluate the rest
  return (env'', val : vals)                                            --Accumulate the evaluated arguments and return them

