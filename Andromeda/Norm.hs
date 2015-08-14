{-# LANGUAGE TupleSections #-}

module Andromeda.Norm (
  nf, whnf
) where

import Andromeda.Syntax
import Andromeda.Context
import Andromeda.Common
import Control.Applicative

-- Normalization of expressions.

-- `norm True env e` evaluates expression `e` in environment `env` to a weak head normal form,
-- while `norm False env e` evaluates to normal form.
norm :: (Functor m, Monad m) => Bool -> Context -> Expr -> ErrorT m Expr
norm weak = go
  where
    go ctx e@(e', loc) = case e' of
      Var k -> case lookupDefinition k ctx of
        Nothing -> return e
        Just f  -> go ctx f
      Universe _ -> return e
      Pi a -> (,loc) <$> Pi <$> abstraction ctx a
      Lambda a -> (,loc) <$> Lambda <$> abstraction ctx a
      Subst s f -> go ctx (subst s f)
      App e1 e2 -> do
        f1@(f1', _) <- go ctx e1
        let app = (,loc) <$> App f1 <$> if weak then return e2 else go ctx e2
        case f1' of
          Lambda (_, _, f) -> go ctx $ mkSubst (Dot e2 idsubst) f
          Var _ -> app
          App _ _ -> app
          _ -> normError (snd e2) "Function expected"
      where abstraction c a@(x, t, f) = if weak then return a else (x,,) <$> go c t <*> go (addParameter x t c) f

-- `nf ctx e` computes the normal form of expression `e`.
nf :: (Functor m, Monad m) => Context -> Expr -> ErrorT m Expr
nf = norm False

-- `whnf ctx e` computes the weak head normal form of expression `e`.
whnf :: (Functor m, Monad m) => Context -> Expr -> ErrorT m Expr
whnf = norm True

