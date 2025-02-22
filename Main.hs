module Main where
import Data.Map ( empty )
import Definitions
--import Typechecker
import Evaluator (evaluate)


main :: IO ()
--main = do
  --evaluateProgram testProgram
--  return ()
main = do
  let program = [
        Declare TInt "x" (Lit (VInt 10)),
        Block [
          Declare TInt "y" (Lit (VInt 20)),
          Print (Get "y") -- Expected: 20
        ],
        Print (Get "x"), -- Expected: 10
        --Print (Get "y"), -- Should give a compile error (out of scope)
        Block [
          Declare TInt "x" (Lit (VInt 50)),
          Print (Get "x") -- Expected: 50 (inner x)
        ],
        Print (Get "x") -- Expected: 10 (outer x)
        ]
  evaluate program

--testProgram :: Expr
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

testProgram = 
  Seq
  [ 
    Declare TInt "x" (Lit (VInt 10))                     
  , Procedure "add" [(TInt, "y"), (TInt, "z")]                 
      (Seq [
        Set "x" (BinOp Add (Get "x") (Get "y"))
      , Print (Get "x")
      , Set "x" (BinOp Add (Get "x") (Get "z"))
      , Print (Get "x")
      ])
  , Call "add" [Lit (VInt 5), Lit (VInt 7)]                            
  , Print (Get "x")                                   
  ]

testProgram = 
  Seq
    [ Declare TInt "i" (Lit (VInt 0)),
      Declare TInt "j" (Lit (VInt 0)),
      While (BinOp Lt (Get "i") (Lit (VInt 3)))
        (Seq
          [ Set "j" (Lit (VInt 0)),
            While (BinOp Lt (Get "j") (Lit (VInt 2)))
              (Seq
                [ Print (BinOp Add (Get "i") (Get "j")),
                  Set "j" (BinOp Add (Get "j") (Lit (VInt 1)))
                ]),
            Set "i" (BinOp Add (Get "i") (Lit (VInt 1)))
          ])
    ]

testProgram = 
  Seq
    [ Function "factorial" [(TInt, "n")] TInt
      (Seq 
        [ IfElse (BinOp Eq (Get "n") (Lit (VInt 0)))
          (Seq 
            [Return (Lit (VInt 1))]
          )
          (Seq 
            [Return (BinOp Mul (Get "n") (Call "factorial" [BinOp Sub (Get "n") (Lit (VInt 1))]))]
          )
        ]
      )
    , Print (Call "factorial" [Lit (VInt 5)])
    ]
testProgram = 
  Seq
    [ Declare TInt "i" (Lit (VInt 0)),
      Declare TInt "j" (Lit (VInt 0)),
      While (BinOp Lt (Get "i") (Lit (VInt 3)))
        (Seq
          [ Set "j" (Lit (VInt 0)),
            While (BinOp Lt (Get "j") (Lit (VInt 2)))
              (Seq
                [ Print (BinOp Add (Get "i") (Get "j")),
                  Set "j" (BinOp Add (Get "j") (Lit (VInt 1)))
                ]),
            Set "i" (BinOp Add (Get "i") (Lit (VInt 1)))
          ])
    ]
testProgram = 
  Seq [
    Declare TInt "x" (Lit (VInt 10)),    -- Declare x and assign 10
    IfElse (BinOp Gt (Get "x") (Lit (VInt 5))) (
        Seq [
            Declare TInt "x" (Lit (VInt 20)),  -- Shadow x inside the if branch
            Print (Get "x")                    -- Print the shadowed x (should print 20)
        ]
    ) (
        Print (Get "x")                    -- This branch won't be executed
    ),
    Print (Get "x")                        -- Print outer x (should print 10)
  ]

-}

      
{-

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

typecheckProgram :: Expr -> Either String Type
typecheckProgram program = 
  let (_, finalType) = typecheck ([empty], empty) program
  in case finalType of
    Left err -> Left err
    Right TNone -> Right TNone
    Right _ -> Left "Type error: incorrect type for program"

-}


--Interpreter sections
--evaluateProgram :: Expr -> IO (Value, Environment)
--evaluateProgram = evaluate ([empty], empty)


