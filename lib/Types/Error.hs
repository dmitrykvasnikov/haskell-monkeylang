module Types.Error where

type Pos = (Int, Int)

type LinePos = Int

type ErrorMessage = String

type ErrorSource = String

data Error = InternalError
           | LexerError Pos ErrorMessage ErrorSource
           | ParserError Pos ErrorMessage ErrorSource

instance Show Error where
  show (LexerError (l, c) msg src) = "Lexer error at line " <> show l <> ", column " <> show c <> "\n" <> msg <> "\nSource:\n" <> src <> "\n"
  show (ParserError (l, c) msg src) = "Parser error at line " <> show l <> ", column " <> show c <> "\n" <> msg <> "\nSource:\n" <> src <> "\n"
  show InternalError = "Internal error for Monoid instance"

instance Semigroup Error where
  err1 <> err2 =
    let (l1, c1) = getPos err1
        (l2, c2) = getPos err2
     in if l1 == l2
          then (if c2 >= c1 then err2 else err1)
          else (if l2 > l1 then err2 else err1)

instance Monoid Error where
  mempty = InternalError
  mappend = (<>)

getPos :: Error -> (Int, Int)
getPos InternalError            = (0, 0)
getPos (LexerError (l, c) _ _)  = (l, c)
getPos (ParserError (l, c) _ _) = (l, c)
