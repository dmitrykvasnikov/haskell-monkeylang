module Types.Error where

type Pos = (Int, Int)

type Length = Int

type ErrorMessage = String

type ErrorSource = String

data Error = InternalError
           | LexerError Pos ErrorMessage ErrorSource

instance Show Error where
  show (LexerError (l, c) msg src) = "Lexer error at line " <> show l <> ", colume " <> show c <> "\n" <> msg <> "\nSource line: \n" <> src
  show InternalError = "Internal error for Monoid instance"

instance Semigroup Error where
  err1 <> err2 =
    let c1 = getPos err1
        c2 = getPos err2
     in if c2 > c1 then err2 else err1

instance Monoid Error where
  mempty = InternalError
  mappend = (<>)

getPos :: Error -> Int
getPos InternalError           = 0
getPos (LexerError (_, c) _ _) = c
