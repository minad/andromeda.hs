module Andromeda.Syntax (
  Expr,
  Expr'(..),
  Abstraction,
  Substitution(..),
  shift,
  subst,
  occurs,
  idsubst,
  mkSubst,
  mkUniverse,
  mkPi
) where

import Andromeda.Common

-- Abstract syntax of internal expressions.

-- Abstract syntax of expressions, where de Bruijn indices are used to represent variables.
type Expr = (Expr', Location)

data Expr' = Var Int                 -- de Briujn index
           | Subst Substitution Expr -- Explicit substitution
           | Universe Int
           | Pi Abstraction
           | Lambda Abstraction
           | App Expr Expr

-- An abstraction [(x,t,e)] indicates that [x] of type [t] is bound in [e].
-- We also keep around the original name [x] of the bound variable for pretty-printing purposes.
type Abstraction = (Variable, Expr, Expr)

-- Explicit substitutions.
data Substitution = Shift Int
                  | Dot Expr Substitution

-- Expression constructors wrapped in "nowhere" locations.
mkVar :: Int -> Expr
mkVar k = nowhere $ Var k

mkSubst :: Substitution -> Expr -> Expr
mkSubst s e = nowhere $ Subst s e

mkUniverse :: Int -> Expr
mkUniverse u = nowhere $ Universe u

mkPi :: Abstraction -> Expr
mkPi a = nowhere $ Pi a

-- The identity substiution.
idsubst :: Substitution
idsubst = Shift 0

-- `shift k e` shifts the indices in `e` by `k` places.
shift :: Int -> Expr -> Expr
shift k = mkSubst (Shift k)

-- `compose s t` composes explicit subtitutions `s` and `t`, i.e.,
-- we have `subst (compose s t) e = subst s (subst t e)`.
compose :: Substitution -> Substitution -> Substitution
compose s (Shift 0) = s
compose (Dot _ s) (Shift m) = compose s $ Shift (m - 1)
compose (Shift m) (Shift n) = Shift (m + n)
compose s (Dot e t) = Dot (mkSubst s e) (compose s t)

-- `occurs k e` returns `True` when variable `Var k` occurs freely in `e`.
occurs :: Int -> Expr -> Bool
occurs k (e, _) = case e of
  Var m -> m == k
  Subst s f -> occurs k (subst s f)
  Universe _ -> False
  Pi a -> abstraction k a
  Lambda a -> abstraction k a
  App e1 e2 -> occurs k e1 || occurs k e2
  where abstraction j (_, e1, e2) = occurs j e1 || occurs (j + 1) e2

-- `subst s e` applies explicit substitution `s` in expression `e`. It does so
-- lazily, i.e., it does just enough to expose the outermost constructor of `e`.
subst :: Substitution -> Expr -> Expr
subst s e@(e', loc) = go s e'
  where go (Shift m) (Var k) = (Var (k + m), loc)
        go (Dot a _) (Var 0) = subst idsubst a
        go (Dot _ t) (Var k) = subst t (Var (k - 1), loc)
        go u (Subst t f) = subst u (subst t f)
        go _ (Universe _) = e
        go t (Pi a) = (Pi (abstraction t a), loc)
        go t (Lambda a) = (Lambda (abstraction t a), loc)
        go t (App e1 e2) = (App (mkSubst t e1) (mkSubst t e2), loc)
        abstraction t (x, e1, e2) = (x, mkSubst t e1, mkSubst (Dot (mkVar 0) (compose (Shift 1) t)) e2)
