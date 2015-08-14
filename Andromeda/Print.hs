module Andromeda.Print (
  printExpr,
  printContext,
  printError
) where

import Andromeda.Common
import Andromeda.Syntax
import Andromeda.Context
import Data.Char

-- Pretty-printing of expressions

-- Given a variable `x` and a list of variable names `xs`, find a variant of `x` which
-- does not appear in `xs`.
refresh :: String -> [String] -> String
refresh x xs = if notElem x xs then x else loop k
  where (y, k) = split x
        loop j = let z = y ++ show j in if elem z xs then loop (j + 1) else z
        split s = if null a then (s, 0) else (reverse b, read (reverse a) :: Int)
          where (a, b) = break isDigit (reverse s)

-- prints expression
printExpr :: Context -> Expr -> String
printExpr ctx = go (contextNames ctx)
    where go xs (e, _) =
              case e of
                Var k -> xs !! k
                Subst s f -> go xs (subst s f)
                Universe u -> "Type " ++ show u
                Pi a -> printPi a
                Lambda a -> printLambda a
                App e1 e2 -> "(" ++ go xs e1 ++ " " ++ go xs e2 ++ ")"
              where printPi (y, e1, e2) =
                        if occurs 0 e2
                          then let x = refresh y xs in "forall " ++ x ++ ": " ++ go xs e1 ++ ", " ++ go (x:xs) e2
                          else "(" ++ go xs e1 ++ " -> " ++ go ("_":xs) e2 ++ ")"
                    printLambda (y, e1, e2) = let x = if occurs 0 e2 then refresh y xs else "_" in
                                              "fun (" ++ x ++ " : " ++ go xs e1 ++ ") => " ++ go (x:xs) e2

printContext :: Context -> String
printContext ctx = unlines $ reverse $ zipWith3 decl (contextNames ctx) (contextDecls ctx) [0..]
    where decl name (Parameter t) k = name ++ " : " ++ printExpr ctx (shift (k + 1) t)
          decl name (Definition t e) k = "[" ++ name ++ " = " ++ printExpr ctx (shift (k + 1) e) ++ "] : " ++ printExpr ctx (shift (k + 1) t)

printError :: Error -> String
printError (Error (Location name sl sc el ec) typ msg)
    | sl == el && el == ec = typ ++ " at \"" ++ name ++ "\", line " ++ show sl ++ ", column " ++ show sc ++ "\n" ++ msg
    | otherwise = typ ++ " at \"" ++ name ++ "\", line " ++ show sl ++ ", column " ++ show sc ++
                  " to line " ++ show el ++ ", column " ++ show ec ++ "\n" ++ msg
printError (Error Nowhere typ msg) = typ ++ "\n" ++ msg
