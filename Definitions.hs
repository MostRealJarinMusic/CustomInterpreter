module Definitions (Expr(..), BinOp(..), UnOp(..), Value(..), Type(..), Variables, Functions, Environment, Token(..)) where
import Data.Map (Map)

--Here, we define the AST
--Loosely based on Haskell's Core
data Expr 
  = Set String Expr --Assignment
  | Declare Type String Expr --Declaration
  | Get String --Access
  | Lit Value  --Literals
  | BinOp BinOp Expr Expr --Binary operations
  | UnOp UnOp Expr --Unary operations
  | IfElse Expr Expr Expr --If-else statements
  | Seq [Expr] --Sequence
  | While Expr Expr --While loops
  | DoWhile Expr Expr --Do-while loops
  | Skip -- Skip
  | Print Expr --Printing
  | Function String [(Type, String)] Type Expr --Function declaration
  | Return Expr --Returning a value from a function
  | Call String [Expr] --Calling a function
  deriving (Show)

--Defining binary operations
data BinOp 
  = Add | Mul | Sub | Div | Mod 
  | Eq | Neq | Lt | Lte | Gt | Gte 
  | And | Or  
  deriving (Show)

--Defining unary operations
data UnOp 
  = Neg
  | Not
  deriving (Show)

--Defining values with wrappers
data Value 
  = VInt Int
  | VString String
  | VBool Bool
  | VGeneric String--Generic types - allowing polymorphism
  | VNone
  | VError
  deriving (Eq, Show)

--Defining data types
data Type 
  = TInt
  | TString
  | TBool 
  | TGeneric
  | TNone
  deriving (Eq, Show)

--Defining variables as a dictionary of 'names' to 'type-values'
type Variables = Map String (Type, Value)

--Defining functions as a dictionary of 'names' to 'parameters-type-expressions'
type Functions = Map String ([(Type, String)], Type, Expr)

--Defining the environment
type Environment = (Variables, Functions)

--Defining patterns to allow for pattern-matching
data Pattern
  = PInt Int
  | PString String
  | PBool Bool
  | PWildcard
  deriving (Eq, Show)




--Defining tokens for the tokenizer and parse to work with
data Token 
  = TokInt Int
  | TokString String
  | TokBool Bool
  | TokEOF
  | TokKeyword String
  | TokIdentifier String
  | TokSymbol String
  | TokLeftBracket
  | TokRightBracket
  | TokLeftBrace
  | TokRightBrace
  | TokSemicolon
  deriving (Show, Eq)