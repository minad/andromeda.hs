{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Andromeda.Common (
  Variable,
  Location(..),
  nowhere,
  Error(..),
  ErrorT,
  syntaxError,
  typeError,
  normError
) where

import Control.Monad.Except

-- Definitions in common use

-- Variable names
type Variable = String

-- Location in source code. For each type in the abstract syntax we define two versions
-- `T` and `T'`. The former is the latter with a location tag. For example, `Expr = (Expr', Location)`
-- and `Expr'` is the type of expressions (without locations).
data Location = Location String Int Int Int Int -- delimited location
              | Nowhere -- unknown location

-- `nowhere e` is the expression `e` without a source location. It is used when
-- an expression is generated and there is not reasonable location that could be
-- assigned to it.
nowhere :: a -> (a, Location)
nowhere = (,Nowhere)

-- Error reporting.

-- Error message with type and location
data Error = Error Location String String

type ErrorT = ExceptT Error

locError :: MonadError Error m => Location -> String -> String -> m a
locError loc typ msg = throwError $ Error loc typ msg

syntaxError, typeError, normError :: MonadError Error m => Location -> String -> m a
syntaxError  loc = locError loc "Syntax error"
typeError    loc = locError loc "Type error"
normError    loc = locError loc "Normalization error"
