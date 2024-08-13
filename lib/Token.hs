module Token where

type TokenType = String

type Literal = String

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
