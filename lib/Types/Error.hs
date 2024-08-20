module Types.Error where

import           Control.Applicative
import           Control.Monad

type ErrorMessage = String

data Error = LexerError ErrorMessage
           | ExpressionError ErrorMessage
           | StatementError ErrorMessage
           -- to use for Alternative empty
           | InternalError

instance Show Error where
  show (LexerError msg)      = "Lexer error: " <> msg
  show (ExpressionError msg) = "Expression Parser error: " <> msg
  show (StatementError msg)  = "Statement Parser error: " <> msg
  show InternalError         = "Internal error"

instance Alternative (Either Error) where
  empty = Left InternalError
  res@(Right _) <|> _        = res
  (Left _) <|> res@(Right _) = res
  err@(Left _) <|> _         = err

instance MonadPlus (Either Error) where
  mzero = empty
  mplus = (<|>)
