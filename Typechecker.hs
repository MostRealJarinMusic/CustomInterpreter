module Typechecker (typecheck, TypeMap) where
import Prelude hiding (lookup)
import Data.Map ( Map, insert, lookup, empty )
import Definitions

type TypeMap = Map String Type

--Typechecking
typecheck :: TypeMap -> Expr -> (TypeMap, Either String Type)
--Literals
typecheck vars (Lit (VInt    _)) = (vars, Right TInt)
typecheck vars (Lit (VBool   _)) = (vars, Right TBool)
typecheck vars (Lit (VString _)) = (vars, Right TString)
--Declarations
typecheck vars (Declare varType name expr) =
  let (vars', exprType) = typecheck vars expr
  in case exprType of
    Left err -> (vars', Left err)
    Right exprType' -> do
      if exprType' == varType
      then (insert name varType vars', Right TNone)
      else (vars', Left $ "Type error in declaration: variable '" ++ name ++ "' declared as " ++ show varType ++ " but expression evaluates to " ++ show exprType')
--Assignment
typecheck vars (Set name expr) = do
  let (vars', exprType) = typecheck vars expr
  case exprType of
    Left err -> (vars', Left err)
    Right exprType' -> 
      case lookup name vars of 
        Just existingType | existingType /= exprType' -> 
          (vars', Left $ "Type error in assignment: variable '" ++ name ++ "' has type " ++ show existingType ++ " but the expression evaluates to " ++ show exprType )
        Nothing -> (vars', Left $ "Compile error in assignment: variable '" ++ name ++ "' not found" )
        _ -> (vars', Right TNone)
--Access
typecheck vars (Get name) = do
  case lookup name vars of
    Nothing -> (vars, Left $ "Compile error in access: variable '" ++ name ++ "' not found" )
    Just existingType -> (vars, Right existingType)
--Binary operators
typecheck vars (BinOp op expr1 expr2) = do
  let (vars', exprType1) = typecheck vars expr1
  let (vars'', exprType2) = typecheck vars' expr2
  case (op, exprType1, exprType2) of
    (Add, Right TInt,    Right TInt)    -> (vars'', Right TInt)
    (Add, Right TString, Right TString) -> (vars'', Right TString)
    (Mul, Right TInt,    Right TInt)    -> (vars'', Right TInt)
    (Sub, Right TInt,    Right TInt)    -> (vars'', Right TInt)
    (Div, Right TInt,    Right TInt)    -> (vars'', Right TInt)
    (Mod, Right TInt,    Right TInt)    -> (vars'', Right TInt)
    (Eq,  Right TInt,    Right TInt)    -> (vars'', Right TBool)
    (Neq, Right TInt,    Right TInt)    -> (vars'', Right TBool)
    (Lt,  Right TInt,    Right TInt)    -> (vars'', Right TBool)
    (Lte, Right TInt,    Right TInt)    -> (vars'', Right TBool)
    (Gt,  Right TInt,    Right TInt)    -> (vars'', Right TBool)
    (Gte, Right TInt,    Right TInt)    -> (vars'', Right TBool)
    (Eq,  Right TBool,   Right TBool)   -> (vars'', Right TBool)
    (Neq, Right TBool,   Right TBool)   -> (vars'', Right TBool)
    (Lt,  Right TBool,   Right TBool)   -> (vars'', Right TBool)
    (Lte, Right TBool,   Right TBool)   -> (vars'', Right TBool)
    (Gt,  Right TBool,   Right TBool)   -> (vars'', Right TBool)
    (Gte, Right TBool,   Right TBool)   -> (vars'', Right TBool)
    (Eq,  Right TString, Right TString) -> (vars'', Right TBool)
    (Neq, Right TString, Right TString) -> (vars'', Right TBool)
    (And, Right TBool,   Right TBool)   -> (vars'', Right TBool)
    (Or,  Right TBool,   Right TBool)   -> (vars'', Right TBool)
    (_,   Left err1,     Left err2)     -> (vars'', Left $ err1 ++ " and " ++ err2)
    (_,   Left err,      _)             -> (vars'', Left err)
    (_,   _,             Left err)      -> (vars'', Left err)
    _                                   -> (vars'', Left $ "Type error in binary operator: " ++ show op ++ " cannot be applied to types " ++ show exprType1 ++ " and " ++ show exprType2)
--Unary operators
typecheck vars (UnOp op expr) = do
  let (vars', exprType) = typecheck vars expr
  case (op, exprType) of
    (Neg, Right TInt)  -> (vars', Right TInt)
    (Not, Right TBool) -> (vars', Right TBool)
    _                  -> (vars', Left $ "Type error in unary operator: incorrect type for " ++ show op)

typecheck vars (IfElse pred expr1 expr2) =
  --checkThrough vars TNone [(pred, TBool, "if-else statement"), (expr1, TNone, "if block"), (expr2, TNone, "else block")]
  let (vars', predType) = typecheck vars pred
      (vars'', expr1Type) = typecheck vars' expr1
      (vars''', expr2Type) = typecheck vars'' expr2
  in case predType of
    Left err -> (vars''', Left err)
    Right TBool -> 
      case (expr1Type, expr2Type) of
        (Right TNone, Right TNone) -> (vars''', Right TNone)
        (Right _, Right _)         -> (vars''', Left "Type error in if-else statement: incorrect type within if-else blocks")
        (Left err, _)              -> (vars''', Left err)
        (_, Left err)              -> (vars''', Left err)
    Right _ -> (vars''', Left "Type error in if-else statement: condition in if-else statement must be a boolean")


typecheck vars (Seq []) = (vars, Right TNone)
typecheck vars (Seq (expr:exprs)) =
  --checkThrough vars TNone [(expr, TNone, "sequence block")]
  let (vars', exprType) = typecheck vars expr
  in case exprType of
    Left err -> (vars', Left err)
    Right TNone -> typecheck vars' (Seq exprs)
    Right _ -> (vars', Left "Type error in sequence: incorrect type within sequence")

typecheck vars (While pred expr) = 
  --checkThrough vars TNone [(pred, TBool, "while condition"), (expr, TNone, "while loop")]
  let (vars', predType) = typecheck vars pred
      (vars'', exprType) = typecheck vars' expr
  in case predType of
    Left err -> (vars'', Left err)
    Right TBool -> 
      case exprType of
        Left err -> (vars'', Left err)
        Right TNone -> (vars'', Right TNone)
        Right _     -> (vars'', Left "Type error in while loop: incorrect type within while loop")
    Right _ -> (vars'', Left "Type error in while loop: condition in while loop must be a boolean")


typecheck vars (DoWhile pred expr) = 
  let (vars', predType) = typecheck vars pred
      (vars'', exprType) = typecheck vars' expr
  in case predType of
    Left err -> (vars'', Left err)
    Right TBool -> 
      case exprType of
        Left err -> (vars'', Left err)
        Right TNone -> (vars'', Right TNone)
        Right _     -> (vars'', Left "Type error in while loop: incorrect type within do-while loop")
    Right _ -> (vars'', Left "Type error in while loop: condition in do-while loop must be a boolean")

typecheck vars (Print expr) = do
  let (vars', exprType) = typecheck vars expr
  case exprType of 
    Left error -> (vars', Left error)
    Right _    -> (vars', Right TNone)

typecheck vars x = (vars, Left $ "Syntax error: unsupported " ++ show x ++ " operation")


