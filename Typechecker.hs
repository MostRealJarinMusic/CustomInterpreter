module Typechecker () where
import Prelude hiding (lookup)
import Data.Map ( Map, insert, lookup, empty, fromList, toList, union )
import Definitions
import Utility



type FunctionTypes = Map String ([Type], Type)
--type VariableTypes = Map String Type
--type ScopedVariableTypes = [VariableTypes]
type EnvironmentTypes = (ScopedVariables, FunctionTypes)



typecheckExpr :: Environment -> Expr -> Either String Type
--Literals - values to types
typecheckExpr env (Lit (VInt    _)) = Right TInt
typecheckExpr env (Lit (VBool   _)) = Right TBool
typecheckExpr env (Lit (VString _)) = Right TString




{-
--Typechecking
typecheck :: EnvironmentTypes -> Expr -> (EnvironmentTypes, Either String Type)
--Literals - mapping values to types
typecheck env (Lit (VInt    _)) = (env, Right TInt)
typecheck env (Lit (VBool   _)) = (env, Right TBool)
typecheck env (Lit (VString _)) = (env, Right TString)

--Declarations
typecheck env@(vars, funcs) (Declare varType name expr) =
  let (env', exprType) = typecheck env expr
  in case exprType of
    Left err -> (env', Left err)                                                                                                                                      --Propagate errors
    Right exprType' -> do
      if exprType' == varType
      then ((insertVariable name (varType, undefined) vars, funcs), Right TNone)                                                                                      --Insert the variable into the scope
      else (env', Left $ "Type error in declaration: variable '" ++ name ++ "' declared as " ++ show varType ++ " but expression evaluates to " ++ show exprType')    --Mismatched type with declaration
      
--Assignment
typecheck env@(vars, funcs) (Set name expr) =
  let (env', exprType) = typecheck env expr
  in case exprType of
    Left err -> (env', Left err)                                                                                                                                              --Propagate errors
    Right exprType' -> 
      case lookupVariable name vars of 
        Just (existingType, _) | existingType /= exprType' ->                                                                                                                 --Mismatched types 
          (env', Left $ "Type error in assignment: variable '" ++ name ++ "' has type " ++ show existingType ++ " but the expression evaluates to " ++ show exprType )
        Nothing -> (env', Left $ "Compile error in assignment: variable '" ++ name ++ "' not found" )                                                                         --Variable doesn't exist
        _ -> (env', Right TNone)                                                                                                                                              --Continue
--Access
typecheck env@(vars, funcs) (Get name) = do
  case lookupVariable name vars of
    Nothing -> (env, Left $ "Compile error in access: variable '" ++ name ++ "' not found" )                                                         --Variable doesn't exist
    Just (existingType, _) -> (env, Right existingType)                                                                                              --Return the variable's type

--Binary operators
typecheck env (BinOp op expr1 expr2) =
  let 
    (env', exprType1) = typecheck env expr1
    (env'', exprType2) = typecheck env' expr2                                                                           --Evaluate both operands
  in case (op, exprType1, exprType2) of
    (Add, Right TInt,    Right TInt)    -> (env'', Right TInt)
    (Add, Right TString, Right TString) -> (env'', Right TString)
    (Mul, Right TInt,    Right TInt)    -> (env'', Right TInt)
    (Sub, Right TInt,    Right TInt)    -> (env'', Right TInt)
    (Div, Right TInt,    Right TInt)    -> (env'', Right TInt)
    (Mod, Right TInt,    Right TInt)    -> (env'', Right TInt)
    (Eq,  Right TInt,    Right TInt)    -> (env'', Right TBool)
    (Neq, Right TInt,    Right TInt)    -> (env'', Right TBool)
    (Lt,  Right TInt,    Right TInt)    -> (env'', Right TBool)
    (Lte, Right TInt,    Right TInt)    -> (env'', Right TBool)
    (Gt,  Right TInt,    Right TInt)    -> (env'', Right TBool)
    (Gte, Right TInt,    Right TInt)    -> (env'', Right TBool)
    (Eq,  Right TBool,   Right TBool)   -> (env'', Right TBool)
    (Neq, Right TBool,   Right TBool)   -> (env'', Right TBool)
    (Lt,  Right TBool,   Right TBool)   -> (env'', Right TBool)
    (Lte, Right TBool,   Right TBool)   -> (env'', Right TBool)
    (Gt,  Right TBool,   Right TBool)   -> (env'', Right TBool)
    (Gte, Right TBool,   Right TBool)   -> (env'', Right TBool)
    (Eq,  Right TString, Right TString) -> (env'', Right TBool)
    (Neq, Right TString, Right TString) -> (env'', Right TBool)
    (And, Right TBool,   Right TBool)   -> (env'', Right TBool)
    (Or,  Right TBool,   Right TBool)   -> (env'', Right TBool)
    (_,   Left err1,     Left err2)     -> (env'', Left $ err1 ++ " and " ++ err2)
    (_,   Left err,      _)             -> (env'', Left err)
    (_,   _,             Left err)      -> (env'', Left err)
    _                                   -> (env'', Left $ "Type error in binary operator: " ++ show op ++ " cannot be applied to types " ++ show exprType1 ++ " and " ++ show exprType2)
--Unary operators
typecheck env (UnOp op expr) =
  let (env', exprType) = typecheck env expr                                                                               --Evaluate the operand
  in case (op, exprType) of
    (Neg, Right TInt)  -> (env', Right TInt)
    (Not, Right TBool) -> (env', Right TBool)
    _                  -> (env', Left $ "Type error in unary operator: incorrect type for " ++ show op)


--Sequence
typecheck env (Seq []) = (env, Right TNone)
typecheck env (Seq sequence@(expr:exprs)) =
  let (env', exprType) = typecheck env expr
  in case exprType of
    Left err -> (env', Left err)
    Right TNone -> typecheck env' (Seq exprs)
    Right t -> (env', Right t)

--Blocks
typecheck env (Block []) = (env, Right TNone)  -- Empty block returns TNone
typecheck (vars, funcs) (Block exprs) = do
  let scopedEnv = (enterScope vars, funcs) -- Enter a new scope
      ((scopedVars', funcs'), finalType) = typecheckBlock scopedEnv exprs
      finalEnv = (exitScope scopedVars', funcs') -- Exit the scope
    in (finalEnv, finalType)
  where
    -- Helper function to type check a sequence of expressions in a block
    typecheckBlock :: EnvironmentTypes -> [Expr] -> (EnvironmentTypes, Either String Type)
    typecheckBlock env [] = (env, Right TNone) -- Empty block returns TNone
    typecheckBlock env (expr:exprs) = 
      let (env', typeResult) = typecheck env expr
      in case typeResult of
        Left err   -> (env', Left err) -- Propagate error
        Right TNone -> typecheckBlock env' exprs -- Continue if no return value
        Right t    -> (env', Right t) -- If returning something, stop checking

--If-else statements
typecheck env (IfElse pred expr1 expr2) =
  let (env', predType) = typecheck env pred
      (env'', expr1Type) = typecheck env' expr1
      (env''', expr2Type) = typecheck env' expr2                                                                                       
  in case predType of
    Left err -> (env''', Left err)
    Right TBool -> 
      case (expr1Type, expr2Type) of
        (Right x, Right y) | x == y -> (env''', Right x)
        (Right _, Right _)         -> (env''', Left "Type error in if-else statement: incorrect type within if-else blocks")
        (Left err, _)              -> (env''', Left err)
        (_, Left err)              -> (env''', Left err)
    Right _ -> (env''', Left "Type error in if-else statement: condition in if-else statement must be a boolean")

--While loops
typecheck env (While pred expr) = 
  let (env', predType) = typecheck env pred
      (env'', exprType) = typecheck env' expr
  in case predType of
    Left err -> (env'', Left err)
    Right TBool -> 
      case exprType of
        Left err -> (env'', Left err)
        Right TNone -> (env'', Right TNone)
        Right _     -> (env'', Left "Type error in while loop: incorrect type within while loop")
    Right _ -> (env'', Left "Type error in while loop: condition in while loop must be a boolean")
  
--Do-while loops
typecheck env (DoWhile pred expr) = 
  let (env', predType) = typecheck env pred
      (env'', exprType) = typecheck env' expr
  in case predType of
    Left err -> (env'', Left err)
    Right TBool -> 
      case exprType of
        Left err -> (env'', Left err)
        Right TNone -> (env'', Right TNone)
        Right _     -> (env'', Left "Type error in while loop: incorrect type within do-while loop")
    Right _ -> (env'', Left "Type error in while loop: condition in do-while loop must be a boolean")

--Printing to 'console'
typecheck env (Print expr) =
  let (env', exprType) = typecheck env expr
  in case exprType of 
    Left error -> (env', Left error)
    Right _    -> (env', Right TNone)

typecheck env Skip = (env, Right TNone)

{-
typecheck env@(vars, funcs) (Function name params retType body) = 
  let 
    funcType = (map fst params, retType)                                                --Get the function type
    updatedFuncs = insert name funcType funcs                                           --Add the function to the environment
    paramTypes = fromList [(n, t) | (t, n) <- params]
    --localEnv = (vars `union` paramTypes, updatedFuncs)
    scopedEnv = (enterScope vars, updatedFuncs) 
  in case expectReturn localEnv retType body of
    Left err -> (env, Left $ "Type error in function: " ++ err)
    Right _ -> ((vars, updatedFuncs), Right TNone)
-}

typecheck env@(vars, funcs) (Function name params retType body) = 
  let
    funcType = (map fst params, retType)                                                              --Define the function type signature
    updatedFuncs = insert name funcType funcs                                                         --Add the type signature to the function environment
    
    paramTypes = fromList [(n, (t, undefined)) | (t, n) <- params]                                    --Map the parameter types 
    newVarScope = enterScope vars                                                                     --Enter a new scope for the function
    funcScopes = insertVariables (toList paramTypes) newVarScope                                      --Insert the new parameters into current scope

    funcEnv = (funcScopes, updatedFuncs)                                                              --Alias for the environment before typechecking the function

    (updatedEnv, result) = typecheck funcEnv body                                                     --Typecheck the function body
    (updatedScopes, _) = updatedEnv                                                                   --Unalias the environment after typechecking the function
    finalScopes = exitScope updatedScopes                                                             --Exit the current scope
    finalEnv = (finalScopes, updatedFuncs)                                                            --Alias for the environment after typechecking

  in case result of
    Left err -> (env, Left err)
    Right bodyType -> if bodyType == retType
      then if containsReturn body 
        then (finalEnv, Right TNone)
        else (finalEnv, Left $ "Type error: missing return statement in " ++ name)
      else (finalEnv, Left $ "Type error: expected return type " ++ show retType ++ " but got " ++ show bodyType)


{-
typecheck env@(vars, funcs) (Procedure name params body) = 
  let 
    procType = (map fst params, TNone) 
    updatedFuncs = insert name procType funcs

    paramTypes = fromList [(n, t) | (t, n) <- params]
    localEnv = (vars `union` paramTypes, updatedFuncs)
  in case typecheck localEnv body of 
    (_, Left err) -> (env, Left err)
    _ -> ((fst env, updatedFuncs), Right TNone)
-}
typecheck env@(vars, funcs) (Call name args) = 
  case lookup name funcs of
    Nothing -> (env, Left $ "function '" ++ name ++ "' is not defined")
    Just (paramTypes, retType) ->
      let checkedArgs = map (typecheck env) args
          argTypes = map snd checkedArgs
      in if all isRight argTypes && map fromRight argTypes == paramTypes
        then (env, Right retType)
        else (env, Left $ "Type error: incorrect argument types for function '" ++ name ++ "': expected: " ++ show paramTypes ++ " but got " ++ show (map fromRight argTypes))

  where
    isRight (Right _) = True
    isRight _         = False

    fromRight (Right x) = x 
    fromRight _         = error "Unexpected Left from fromRight"


typecheck env (Return expr) = 
  let (env', exprType) = typecheck env expr
  in case exprType of
    Left error -> (env', Left error)
    Right _ -> (env', exprType)

typecheck vars x = (vars, Left $ "Syntax error: unsupported " ++ show x ++ " operation")


expectReturn :: EnvironmentTypes -> Type -> Expr -> Either String ()
expectReturn env retType body = 
  case hasReturn env retType body of
    Nothing -> Right ()
    Just err -> Left err
  
hasReturn :: EnvironmentTypes -> Type -> Expr -> Maybe String
hasReturn env retType expr = case expr of
  Return retExpr -> 
    case typecheck env retExpr of
      (_, Right retExprType) | retExprType == retType -> Nothing
      (_, Right retExprType)                          -> Just $ "return type mismatch: expected " ++ show retType ++ " but got " ++ show retExprType
      (_, Left err)                                   -> Just err
  Seq []    -> Just "function body has no return statement"
  --Seq exprs -> any (hasReturn env retType) exprs
  Seq (expr:exprs) -> 
    case hasReturn env retType expr of
      Nothing | isReturn expr && not (null exprs) -> Just "unreachable code after return statement"
      Nothing | isReturn expr -> Nothing
      Nothing -> hasReturn env retType (Seq exprs)
      Just err -> Just err
  IfElse _ trueBranch falseBranch ->
    case (hasReturn env retType trueBranch, hasReturn env retType falseBranch) of
      (Nothing, Nothing) -> Nothing
      (Just err, _) -> Just err
      (_, Just err) -> Just err
  While _ whileBlock -> hasReturn env retType whileBlock
  DoWhile _ doWhileBlock -> hasReturn env retType doWhileBlock
  _ -> Just "function does not contain a valid return statement"


isReturn :: Expr -> Bool
isReturn (Return _) = True
isReturn _          = False


containsReturn :: Expr -> Bool
containsReturn (Return _) = True  -- A direct return statement
containsReturn (Block exprs) = any containsReturn exprs  -- Check within a block
containsReturn (Seq exprs) = any containsReturn exprs  -- Check within a sequence
containsReturn (IfElse _ thenBranch elseBranch) = containsReturn thenBranch && containsReturn elseBranch  -- Both branches must return
containsReturn (While _ body) = containsReturn body  -- Ensure loop contains return
containsReturn (DoWhile _ body) = containsReturn body  -- Ensure loop contains return
containsReturn _ = False  -- Other expressions don’t count


-}