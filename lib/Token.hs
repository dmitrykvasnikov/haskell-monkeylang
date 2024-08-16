module Token where

data Token = ASSIGN
           | PLUS
           | MINUS
           | MULT
           | DIV
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
           | ERROR String
           | EOF
  deriving (Show)

instance Eq Token where
  (==) (IDENT _) (IDENT _) = True
  (==) (ERROR _) (ERROR _) = True
  (==) (INT _) (INT _)     = True
  (==) tok1 tok2           = (show tok1) == (show tok2)

instance {-# OVERLAPPING #-} Show [Token] where
  show [] = "No tokens in input stream"
  show (t : ts) = "[" ++ (show t) ++ (foldl (\out tok -> out ++ ", " ++ show tok) "" ts) ++ "]"
