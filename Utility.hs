module Utility (lookupVariable, updateVariable, insertVariable, enterScope, exitScope) where
import Prelude hiding (lookup)
import Data.Map ( Map, insert, lookup, empty, union, fromList, delete, member )
import Definitions


lookupVariable :: String -> ScopedVariables -> Maybe (Type, Value)
lookupVariable _ [] = Nothing
lookupVariable name (scope:rest) =
  case lookup name scope of
    Just var -> Just var
    Nothing  -> lookupVariable name rest

updateVariable :: String -> (Type, Value) -> ScopedVariables -> ScopedVariables
updateVariable name _ [] = error "Variable not found"
updateVariable name typeVal (scope:rest) = 
  if member name scope 
  then insert name typeVal scope : rest 
  else scope : updateVariable name typeVal rest

insertVariable :: String -> (Type, Value) -> ScopedVariables -> ScopedVariables
insertVariable _ _ [] = error "No scope to insert variable"
insertVariable name typeVal (scope:rest) = insert name typeVal scope : rest

enterScope :: ScopedVariables -> ScopedVariables
enterScope vars = empty : vars

exitScope :: ScopedVariables -> ScopedVariables
exitScope [] = error "No scope to exit"
exitScope (scope:rest) = rest
