module Definitions (Expr(..), BinOp(..), UnOp(..), Value(..), Type(..), Variables) where
import Data.Map (Map)

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