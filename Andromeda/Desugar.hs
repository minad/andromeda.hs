{-# LANGUAGE TupleSections, FlexibleContexts #-}

module Andromeda.Desugar (
  desugar
) where

import qualified Andromeda.Input as In
import qualified Andromeda.Syntax as Syn
import Andromeda.Common
import Andromeda.Context
import Data.List
import Control.Applicative

-- Desugaring of input syntax to internal syntax.

-- `desugar ctx e` converts an expression of type `In.expr` to type
-- `Syn.expr` by replacing names in `e` with de Bruijn indices.
desugar :: (Functor m, Monad m) => Context -> In.Expr -> ErrorT m Syn.Expr
desugar ctx = go (contextNames ctx)
  where go xs (e, loc) = case e of
          In.Var x -> do
            i <- index x
            return (Syn.Var i, loc)
          In.Universe u -> return (Syn.Universe u, loc)
          In.Pi a -> (,loc) <$> Syn.Pi <$> abstraction a
          In.Lambda a -> (,loc) <$> Syn.Lambda <$> abstraction a
          In.App e1 e2 -> (,loc) <$> (Syn.App <$> go xs e1 <*> go xs e2)
          where abstraction (x, t, f) = (x,,) <$> go xs t <*> go (x:xs) f
                index x = case elemIndex x xs of
                  Nothing -> typeError loc ("unknown identifier " ++ x)
                  Just i -> return i
