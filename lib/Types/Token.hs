module Types.Token where

-- Tokens for lexer and parser
data Token -- Keywords
           = LET
           | RETURN
           | FUNCTION
           | IF
           | THEN
           | TRUE
           | FALSE
           | ELSE
           -- Syntax
           | SEMICOLON
           | COMMA
           | EOF
           | LPAREN
           | RPAREN
           | LBRACE
           | RBRACE
           -- Identifiers
           | ID String
           | INT Int
           | STRING String
           -- Unary operators
           | NOT
           | NEG
           -- Binary operators
           | PLUS
           | MINUS
           | MULT
           | DIV
           | EQ
           | NOTEQ
           | GRT
           | LST
           | ASSIGN
           -- System for internal usage
           | ILLEGAL
           | NOTOKEN
  deriving (Show)

instance Eq Token where
  (==) (ID _) (ID _)         = True
  (==) (INT _) (INT _)       = True
  (==) (STRING _) (STRING _) = True
  (==) tok1 tok2             = show tok1 == show tok2

keywords :: [(String, Token)]
keywords =
  [ ("let", LET),
    ("return", RETURN),
    ("fn", FUNCTION),
    ("if", IF),
    ("then", THEN),
    ("true", TRUE),
    ("false", FALSE),
    ("else", ELSE)
  ]
