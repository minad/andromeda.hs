module Andromeda.Context (
  Declaration(..),
  Context(..),
  emptyContext,
  lookupTy,
  lookupDefinition,
  addParameter,
  addDefinition
) where

import Andromeda.Syntax

--import Control.Monad.Reader
--type ContextT m = ReaderT Context (ErrorT m)

-- Context management

-- A context is represented as an associative list which maps a variable `x` to a pair
-- `(t, e)` where `t` is its type and `e` is its value (optional).

-- The entries in the context are declarations of parameters or definitions.
-- A parameter declaration carries its type, while a definition carries the type and
-- the defining expression.
data Declaration = Parameter Expr
                 | Definition Expr Expr

-- A context consists of a list of names, used for pretty-printing and
-- desugaring of variable names to de Bruijn indices, and a list of
-- declarations.
data Context = Context { contextNames :: [String]
                       , contextDecls :: [Declaration] }

-- On the zeroth day there was the empty context.
emptyContext :: Context
emptyContext = Context [] []

-- `lookupTy k ctx` returns the type of `Var k` in context `ctx`.
lookupTy :: Int -> Context -> Expr
lookupTy k c =
  case contextDecls c !! k of
    Parameter t -> shift (k + 1) t
    Definition t _ -> shift (k + 1) t

-- `lookupDefinition k ctx` returns the definition of `Var k` in context `ctx`.
lookupDefinition :: Int -> Context -> Maybe Expr
lookupDefinition k c =
  case contextDecls c !! k of
    Definition _ e -> Just $ shift (k + 1) e
    Parameter _ -> Nothing

-- `addParameter x t ctx` returns `ctx` with the parameter `x` of type `t`.
addParameter :: String -> Expr -> Context -> Context
addParameter x t ctx = Context (x:contextNames ctx) (Parameter t:contextDecls ctx)

-- `addDefinition x t e ctx` returns `ctx` with `x` of type `t` defined as `e`.
addDefinition :: String -> Expr -> Expr -> Context -> Context
addDefinition x t e ctx = Context (x:contextNames ctx) (Definition t e:contextDecls ctx)
