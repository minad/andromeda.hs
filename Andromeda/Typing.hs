module Andromeda.Typing (
  infer,
  inferUniverse
) where

import Andromeda.Common
import Andromeda.Norm
import Andromeda.Syntax
import Andromeda.Context
import Control.Applicative
import Andromeda.Print

-- Type inference.

-- `infer ctx e` infers the type of expression `e` in context `ctx`.
infer :: (Functor m, Monad m) => Context -> Expr -> ErrorT m Expr
infer ctx (e, _) =
  case e of
    Var k -> return $ lookupTy k ctx
    Universe u -> return $ mkUniverse (u + 1)
    Pi (x, e1, e2) -> do
      u1 <- inferUniverse ctx e1
      u2 <- inferUniverse (addParameter x e1 ctx) e2
      return $ mkUniverse (max u1 u2)
    Subst s f -> infer ctx (subst s f)
    Lambda (x, e1, e2) -> do
      _ <- inferUniverse ctx e1
      t2 <- infer (addParameter x e1 ctx) e2
      return $ mkPi (x, e1, t2)
    App e1 e2 -> do
      (_, s, t) <- inferPi ctx e1
      t2 <- infer ctx e2
      b <- equal ctx s t2
      if b
        then return $ mkSubst (Dot e2 idsubst) t
        else typeError (snd e2) ("The expression has type " ++ printExpr ctx t2 ++ " but " ++ printExpr ctx s ++ " was expected")

-- `inferUniverse ctx t` infers the universe level of type `t` in context `ctx`.
inferUniverse :: (Functor m, Monad m) => Context -> Expr -> ErrorT m Int
inferUniverse ctx t = do
  u <- infer ctx t
  n <- whnf ctx u
  case fst n of
    Universe i -> return i
    _ -> typeError (snd t) ("The expression has type " ++ printExpr ctx u ++ " but it should be a universe")

inferPi :: (Functor m, Monad m) => Context -> Expr -> ErrorT m Abstraction
inferPi ctx e = do
  t <- infer ctx e
  n <- whnf ctx t
  case fst n of
    Pi a -> return a
    _ -> typeError (snd e) ("The expression has type " ++ printExpr ctx t ++ " but it should be a function")

-- `equal ctx e1 e2` determines whether `e1` and `e2` are equal expressions.
equal :: (Functor m, Monad m) => Context -> Expr -> Expr -> ErrorT m Bool
equal ctx e1 e2 = do
  (f1, _) <- whnf ctx e1
  (f2, _) <- whnf ctx e2
  eq f1 f2
  where eq (Var k1) (Var k2) = return $ k1 == k2
        eq (Universe u1) (Universe u2) = return $ u1 == u2
        eq (Pi a1) (Pi a2) = abstraction ctx a1 a2
        eq (Lambda a1) (Lambda a2) = abstraction ctx a1 a2
        eq (App n1 f1) (App n2 f2) = (&&) <$> equal ctx n1 n2 <*> equal ctx f1 f2
        eq _ _ = return False
        abstraction c (x, f1, f2) (_, f1', f2') = (&&) <$> equal c f1 f1' <*> equal (addParameter x f1 c) f2 f2'
