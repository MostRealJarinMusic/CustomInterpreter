module Definitions (Identifier, Expr(..), BinOp(..), UnOp(..), Value(..), Type(..), Variables, ScopedVariables, Functions, Environment, Token(..)) where
import Data.Map (Map)

--Here, we define the AST
--Loosely based on Haskell's Core
type Identifier = String

data Expr 
  = Set Identifier Expr                                       --Assignment
  | Declare Type Identifier Expr                              --Declaration
  | Get Identifier                                            --Access
  | Lit Value                                             --Literals
  | BinOp BinOp Expr Expr                                 --Binary operations
  | UnOp UnOp Expr                                        --Unary operations
  | Seq [Expr]                                            --Sequence - no new variable scopes
  | Block [Expr]                                          --Blocks - introduces a new variable scope
  | IfElse Expr Expr Expr                                 --If-else statements                                  Add Maybe Expr to allow for single if statements
  | While Expr Expr                                       --While loops
  | DoWhile Expr Expr                                     --Do-while loops
  | Skip                                                  --Skip
  | Print Expr                                            --Printing
  | Function Identifier [(Type, Identifier)] Type Expr            --Function declaration
  | Procedure Identifier [(Type, Identifier)] Expr                --Procedure declaration
  | Return Expr                                           --Returning a value from a function
  | Call String [Expr]                                    --Calling a function / procedure
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
  | VGeneric String --Generic types - allowing polymorphism
  | VNone
  | VError
  deriving (Eq, Show)

--Defining data types
data Type 
  = TInt
  | TString
  | TBool 
  | TGeneric --Generic types 
  | TNone
  deriving (Eq, Show)

--Defining variables as a dictionary of 'names' to 'type-values'
type Variables = Map Identifier (Type, Value)
type ScopedVariables = [Variables]

--Defining functions as a dictionary of 'names' to 'parameters-type-expressions'
type Functions = Map Identifier ([(Type, Identifier)], Type, Expr)

--Defining the environment
type Environment = (ScopedVariables, Functions)


--Defining patterns to allow for pattern-matching
data Pattern
  = PInt Int
  | PString String
  | PBool Bool
  | PWildcard
  deriving (Eq, Show)

--Defining tokens for the tokenizer and parser to work with
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