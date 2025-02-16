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