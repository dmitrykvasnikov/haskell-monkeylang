module Token where

data Token = ASSIGN
           | PLUS
           | MINUS
           | ASTERISK
           | SLASH
           | GT
           | LT
           | BANG
           | EQ
           | NOT_EQ
           | COMMA
           | SEMICOLON
           | LPAREN
           | RPAREN
           | LBRACE
           | RBRACE
           | FUNCTION
           | LET
           | IDENT String
           | INT Int
           | TRUE
           | FALSE
           | IF
           | THEN
           | ELSE
           | RETURN
           | ILLEGAL
           | EOF
  deriving (Show)

instance {-# OVERLAPPING #-} Show [Token] where
  show [] = "No tokens generated"
  show (t : ts) = (show t) ++ (foldl (\out tok -> out ++ ", " ++ show tok) "" ts)
