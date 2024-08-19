module Types.Error where

type ErrorMessage = String

data Error = LexerError ErrorMessage
           | ExpressionError ErrorMessage
           | StatementError ErrorMessage

instance Show Error where
  show (LexerError msg)      = "Lexer error: " <> msg
  show (ExpressionError msg) = "Expression Parser error: " <> msg
  show (StatementError msg)  = "Statement Parser error: " <> msg
