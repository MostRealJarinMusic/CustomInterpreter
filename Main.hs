module Main where
import Data.Map ( empty )
import Definitions
import Typechecker
import Evaluator (evaluate)


main :: IO ()
main = do
  --evaluateProgram testProgram
  return ()


testProgram :: Expr
--testProgram = Seq (Set "x" (Lit (VInt 7))) (Seq (Set "y" (Lit (VString "6"))) (Set "z" (BinOp Add (Get "x") (Get "y"))))
--testProgram = Set "x" (BinOp Add (Lit (VInt 7)) (Lit (VString "6")))
--testProgram = Seq (Set "x" (Lit (VInt 42))) (Print (Get "x"))
--testProgram = Seq (Set "msg" (Lit (VString "Hello"))) (Seq (Set "count" (Lit (VInt 3))) (While (BinOp Gt (Get "count") (Lit (VInt 0))) (Seq (Print (Get "msg")) (Set "count" (BinOp Sub (Get "count") (Lit (VInt 1)))))))
--testProgram = Seq (Set "x" (Lit (VInt 10))) (While (BinOp Gt (Get "x") (Lit (VInt 0))) (Seq (Set "x" (BinOp Sub (Get "x") (Lit (VInt 1)))) (Print (Get "x"))))
--testProgram = Print (Get "undefinedVar")
--testProgram = Seq (Set "msg" (Lit (VString "Hello"))) (Seq (Set "count" (Lit (VInt 10))) (DoWhile (BinOp Gt (Get "count") (Lit (VInt 0))) (Seq (Print (Get "msg")) (Set "count" (BinOp Sub (Get "count") (Lit (VInt 1)))) )))
{-
testProgram = 
  Seq 
    [ Declare TString "msg" (Lit (VString "Hello"))
    , Declare TInt "count" (Lit (VInt 10))
    , DoWhile 
        (BinOp Gte (Get "count") (Lit (VInt 0))) 
        (Seq 
            [ Print (Get "count")
            , Set "count" (BinOp Sub (Get "count") (Lit (VInt 1)))
            ]
        )
    ]

testProgram = Seq
        [ Declare TInt "x" (Lit (VInt 10))                      -- Declare global x
        , Function "add" [(TInt, "y")] TNone                   -- Define function add
            (Seq [ Set "x" (BinOp Add (Get "x") (Get "y"))
                 , Return (Lit VNone)
                 ])
        , Call "add" [Lit (VInt 5)]                            -- Call add(5)
        , Print (Get "x")                                      -- Print global x
        ]

testProgram = Seq
        [ Function "factorial" [(TInt, "n")] TInt
            (IfElse (BinOp Eq (Get "n") (Lit (VInt 0)))
              (Return (Lit (VInt 1)))
              (Return (BinOp Mul (Get "n") (Call "factorial" [BinOp Sub (Get "n") (Lit (VInt 1))]))))
        , Print (Call "factorial" [Lit (VInt 10)])
        , Function "sum" [(TInt, "a"), (TInt, "b")] TInt
          (Seq [
            Return (BinOp Add (Get "a") (Get "b"))
          , Print (Lit (VString "Unreachable"))
          ])
        ]
-}
testProgram = Seq
        [ Declare TInt "x" (Lit (VInt 10))                      -- Declare global x
        , Function "add" [(TInt, "y")] TNone                   -- Define function add
            (Seq [ Set "x" (BinOp Add (Get "x") (Get "y"))
                 , Return (Lit VNone)
                 ])
        , Call "add" [Lit (VInt 5)]                            -- Call add(5)
        , Print (Get "x")                                      -- Print global x
        ]

{-
tokenizer :: String -> [Token]
tokenizer = lexer [] . words
 where 
    lexer :: [Token] -> [String] -> [Token]
    lexer acc [] = acc ++ [TokEOF]
    lexer acc (x:xs) 
      | x == ";"                                                                              = lexer (acc ++ [TokSemicolon]) xs
      | x == "("                                                                              = lexer (acc ++ [TokLeftBracket]) xs
      | x == ")"                                                                              = lexer (acc ++ [TokRightBracket]) xs
      | x == "{"                                                                              = lexer (acc ++ [TokLeftBrace]) xs
      | x == "}"                                                                              = lexer (acc ++ [TokRightBrace]) xs
      | all (`elem` "0123456789") x                                                           = lexer (acc ++ [TokInt (read x)]) xs
      | x `elem` ["True", "False"]                                                            = lexer (acc ++ [TokBool (x == "True")]) xs
      | x `elem` ["if", "else", "while", "do", "print", "skip"]                               = lexer (acc ++ [TokKeyword x]) xs
      | x `elem` ["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||", "!"] = lexer (acc ++ [TokSymbol x]) xs
      | head x == '"' && last x == '"'                                                        = lexer (acc ++ [TokString ((init . tail) x)]) xs
      | otherwise                                                                             = lexer (acc ++ [TokIdentifier x]) xs

-}


run :: Expr -> IO ()
run expr = 
  let finalType = typecheckProgram expr
  in case finalType of
    Left err -> do 
      putStrLn err
      return ()
    _ -> do
      evaluateProgram expr
      return ()

--Interpreter sections
evaluateProgram :: Expr -> IO (Value, Environment)
evaluateProgram program = evaluate (empty, empty) program

typecheckProgram :: Expr -> Either String Type
typecheckProgram program = 
  let (_, finalType) = typecheck (empty, empty) program
  in case finalType of
    Left err -> Left err
    Right TNone -> Right TNone
    Right _ -> Left "Type error: incorrect type for program"