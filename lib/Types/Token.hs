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
           | SEMICOLON_
           | COMMA
           | EOF
           | COLON
           | LPAREN
           | RPAREN
           | LBRACE
           | RBRACE
           | LBRACKET
           | RBRACKET
           -- Identifiers
           | ID String
           | INT Int
           | STRING String
           -- Unary operators
           | NOT
           -- Binary operators
           | CONCAT
           | PLUS
           | MINUS
           | MULT
           | DIV
           | EQL
           | NOTEQL
           | GRT
           | LST
           | GRTEQL
           | LSTEQL
           | AND
           | OR
           | ASSIGN
           -- System for internal usage
           | ILLEGAL
           | NOTOKEN
  deriving (Ord)

instance Show Token where
  show LET          = "LET"
  show RETURN       = "RETURN"
  show FUNCTION     = "FUNCTION"
  show IF           = "IF"
  show THEN         = "THEN"
  show TRUE         = "TRUE"
  show FALSE        = "FALSE"
  show ELSE         = "ELSE"
  show COLON        = ":"
  show SEMICOLON    = ";"
  show SEMICOLON_   = ";"
  show COMMA        = ","
  show EOF          = "EOF"
  show LPAREN       = "("
  show RPAREN       = ")"
  show LBRACE       = "{"
  show RBRACE       = "}"
  show LBRACKET     = "["
  show RBRACKET     = "]"
  show (ID var)     = "ID " <> var
  show (INT num)    = "INT " <> show num
  show (STRING str) = "STRING " <> show str
  show NOT          = "!"
  show CONCAT       = "++"
  show PLUS         = "+"
  show MINUS        = "-"
  show MULT         = "*"
  show DIV          = "/"
  show EQL          = "=="
  show NOTEQL       = "!="
  show GRT          = ">"
  show LST          = "<"
  show GRTEQL       = ">="
  show LSTEQL       = "<="
  show ASSIGN       = "="
  show AND          = "&&"
  show OR           = "||"
  show ILLEGAL      = "ILLEGAL"
  show NOTOKEN      = "NOTOKEN"

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
