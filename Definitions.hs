module Definitions (Expr(..), BinOp(..), UnOp(..), Value(..), Type(..), Variables) where
import Data.Map (Map)

--Here, we define the AST
--Loosely based on Haskell's Core
data Expr = Set String Expr 
            | Get String
            | Lit Value
            | BinOp BinOp Expr Expr
            | UnOp UnOp Expr
            | IfElse Expr Expr Expr
            | Seq Expr Expr
            | While Expr Expr
            | DoWhile Expr Expr
            | Skip
            | Print Expr
    deriving (Show)

--Defining binary operations
data BinOp = Add | Mul | Sub | Div | Mod 
            | Eq | Neq | Lt | Lte | Gt | Gte 
            | And | Or  
    deriving (Show)

--Defining unary operations
data UnOp = 
    Neg
    | Not
    deriving (Show)

--Defining values with wrappers
data Value = 
    VInt Int
    | VString String
    | VBool Bool
    | VNone
    | VError
    deriving (Eq, Show)

--Defining data types
data Type = 
    TInt
    | TString
    | TBool 
    | TNone
    deriving (Eq, Show)

--Defining variables as a dictionary of 'names' to 'type-values'
type Variables = Map String (Type, Value)