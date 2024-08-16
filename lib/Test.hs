module Test where

import           Control.Monad.State.Strict
import           Data.List.Split
import           Lexer
import           Parser

test :: FilePath -> IO ()
test fn = do
  (res, exp) <- mkResExp . lines <$> readFile fn
  let r = map (evalState lexer . mkLInputFs) res
  mapM_ (putStrLn . show) r

mkResExp :: [String] -> ([String], [String])
mkResExp content = go ([], []) content
  where
    go (r, e) (r' : e' : rest) = go (r ++ [r'], e ++ [e']) rest
    go res _                   = res
