module Typechecker (typecheck) where
import Prelude hiding (lookup)
import Data.Map ( Map, insert, lookup, empty, fromList, union )
import Definitions


type FunctionTypes = Map String ([Type], Type)
type VariableTypes = Map String Type
type ScopedVariableTypes = [VariableTypes]
type EnvironmentTypes = (VariableTypes, FunctionTypes)

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
    Left err -> (env', Left err)
    Right exprType' -> do
      if exprType' == varType
      then ((insert name varType vars, funcs), Right TNone)
      else (env', Left $ "Type error in declaration: variable '" ++ name ++ "' declared as " ++ show varType ++ " but expression evaluates to " ++ show exprType')
--Assignment
typecheck env@(vars, funcs) (Set name expr) =
  let (env', exprType) = typecheck env expr
  in case exprType of
    Left err -> (env', Left err)
    Right exprType' -> 
      case lookup name vars of 
        Just existingType | existingType /= exprType' -> 
          (env', Left $ "Type error in assignment: variable '" ++ name ++ "' has type " ++ show existingType ++ " but the expression evaluates to " ++ show exprType )
        Nothing -> (env', Left $ "Compile error in assignment: variable '" ++ name ++ "' not found" )
        _ -> (env', Right TNone)
--Access
typecheck env@(vars, funcs) (Get name) = do
  case lookup name vars of
    Nothing -> (env, Left $ "Compile error in access: variable '" ++ name ++ "' not found" )
    Just existingType -> (env, Right existingType)
--Binary operators
typecheck env (BinOp op expr1 expr2) =
  let 
    (env', exprType1) = typecheck env expr1
    (env'', exprType2) = typecheck env' expr2
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
  let (env', exprType) = typecheck env expr
  in case (op, exprType) of
    (Neg, Right TInt)  -> (env', Right TInt)
    (Not, Right TBool) -> (env', Right TBool)
    _                  -> (env', Left $ "Type error in unary operator: incorrect type for " ++ show op)
--If-else statements
typecheck env (IfElse pred expr1 expr2) =
  let (env', predType) = typecheck env pred
      (env'', expr1Type) = typecheck env' expr1
      (env''', expr2Type) = typecheck env'' expr2
  in case predType of
    Left err -> (env''', Left err)
    Right TBool -> 
      case (expr1Type, expr2Type) of
        (Right x, Right y) | x == y -> (env''', Right x)
        (Right _, Right _)         -> (env''', Left "Type error in if-else statement: incorrect type within if-else blocks")
        (Left err, _)              -> (env''', Left err)
        (_, Left err)              -> (env''', Left err)
    Right _ -> (env''', Left "Type error in if-else statement: condition in if-else statement must be a boolean")
--Sequence
typecheck env (Seq []) = (env, Right TNone)
typecheck env (Seq (expr:exprs)) =
  let (env', exprType) = typecheck env expr
  in case exprType of
    Left err -> (env', Left err)
    Right TNone -> typecheck env' (Seq exprs)
    Right _ -> (env', Left "Type error in sequence: incorrect type within sequence")
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

typecheck env@(vars, funcs) (Function name params retType body) = 
  let 
    funcType = (map fst params, retType)
    updatedFuncs = insert name funcType funcs

    paramTypes = fromList [(n, t) | (t, n) <- params]
    localEnv = (vars `union` paramTypes, updatedFuncs)
  in case expectReturn localEnv retType body of
    Left err -> (env, Left $ "Type error in function: " ++ err)
    Right _ -> ((vars, updatedFuncs), Right TNone)

typecheck env@(vars, funcs) (Procedure name params body) = 
  let 
    procType = (map fst params, TNone) 
    updatedFuncs = insert name procType funcs

    paramTypes = fromList [(n, t) | (t, n) <- params]
    localEnv = (vars `union` paramTypes, updatedFuncs)
  in case typecheck localEnv body of 
    (_, Left err) -> (env, Left err)
    _ -> ((fst env, updatedFuncs), Right TNone)

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