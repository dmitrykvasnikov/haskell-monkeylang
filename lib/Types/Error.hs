module Types.Error where

type Pos = (Int, Int)

type ErrorMessage = String

data Error = InternalError
           | LexerError Pos ErrorMessage

instance Show Error where
  show (LexerError pos err) = "Lexer error at position " <> show pos <> "\n" <> err
  show InternalError = "Internal error for Monoid instance"

instance Semigroup Error where
  err1 <> err2 =
    let (_, c1) = getPos err1
        (_, c2) = getPos err2
     in if c2 > c1 then err2 else err1

instance Monoid Error where
  mempty = InternalError
  mappend = (<>)

getPos :: Error -> Pos
getPos InternalError    = (0, 0)
getPos (LexerError p _) = p
