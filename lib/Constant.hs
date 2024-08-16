module Constant where

import           Token

keywords :: [(String, Token)]
keywords =
  [ ("let", LET),
    ("fn", FUNCTION),
    ("return", RETURN),
    ("true", TRUE),
    ("false", FALSE),
    ("if", IF),
    ("then", THEN),
    ("else", ELSE)
  ]

singleCharToken :: [(Char, Token)]
singleCharToken =
  [ ('=', ASSIGN),
    (',', COMMA),
    ('+', PLUS),
    ('-', MINUS),
    ('*', MULT),
    ('/', DIV),
    ('!', BANG),
    ('<', Token.LT),
    ('>', Token.GT),
    (';', SEMICOLON),
    ('(', LPAREN),
    (')', RPAREN),
    ('{', LBRACE),
    ('}', RBRACE)
  ]

singleCharTokenPattern :: String
singleCharTokenPattern = "=,+-*/!<>;(){}"
