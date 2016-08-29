module Lexer where
import  Data.Char

data Token
  = PlusOp
  | MultOp
  | CloseP
  | OpenP
  | IntLit Int
  deriving (Show)


lexer :: String -> [Token]
lexer [] = []
lexer ('(' : restStr) = OpenP : (lexer restStr) 
lexer (')' : restStr) = CloseP : (lexer restStr)
lexer ('+' : restStr) = PlusOp : (lexer restStr)
lexer ('*' : restStr) = MultOp : (lexer restStr) 
lexer (chr : restStr) | (isSpace chr) = lexer restStr
lexer str@(chr : restStr) | (isDigit chr)
  = IntLit (stringToInt 0 digitStr) : (lexer restStr')
  where
     (digitStr, restStr') = break (not. isDigit) str
     -- defining a local function here:
     stringToInt :: Int -> String -> Int
     stringToInt  acc []
       = acc
     stringToInt  acc (c : chs) 
       = stringToInt ((acc * 10)  + (digitToInt c)) chs
-- runtime error for all other characters:
lexer (chr :restString) 
  = error ("lexer: unexpected character: " ++ (show chr))


