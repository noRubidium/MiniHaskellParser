module Lexer where
import  Data.Char

data Token
  = PlusOp
  | MultOp
  | CloseP
  | OpenP
  | Let
  | In
  | Is
  | IntLit Int
  | Ident  String
  deriving (Show)


lexer :: String -> [Token]
lexer [] = []
lexer ('(' : restStr) = OpenP : (lexer restStr) 
lexer (')' : restStr) = CloseP : (lexer restStr)
lexer ('+' : restStr) = PlusOp : (lexer restStr)
lexer ('*' : restStr) = MultOp : (lexer restStr) 
lexer ('l' : 'e' : 't' : restStr) = Let : (lexer restStr)
lexer ('i' : 'n' : restStr) = In : (lexer restStr)
lexer ('=' : restStr) = Is : (lexer restStr)
lexer (chr : restStr) | (isSpace chr) = lexer restStr
lexer str@(chr : restStr) | (isAlpha chr)
  = Ident s: lexer restStr'
  where
    (s,restStr') = break (not. isAlpha) str
    
lexer str@(chr : restStr) | (isDigit chr)
  = IntLit (stringToInt 0 digitStr) : (lexer restStr')
  where
     (digitStr, restStr') = break (not. isDigit) str
     stringToInt :: Int -> String -> Int
     stringToInt  acc []
       = acc
     stringToInt  acc (c : chs) 
       = stringToInt ((acc * 10)  + (digitToInt c)) chs
lexer (chr :restString) 
  = error ("lexer: unexpected character: " ++ (show chr))


