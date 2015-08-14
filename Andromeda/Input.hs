module Andromeda.Input (
  Expr,
  Expr'(..),
  Abstraction,
  Directive,
  Directive'(..),
  Location(..),
  Variable
) where

import Andromeda.Common

-- Abstract syntax of input files.

-- Abstract syntax of expressions as given by the user.
type Expr = (Expr', Location)

data Expr' = Var Variable
           | Universe Int
           | Pi Abstraction
           | Lambda Abstraction
           | App Expr Expr

-- An abstraction `(x,t,e)` indicates that `x` of type `t` is bound in `e`.
type Abstraction = (Variable, Expr, Expr)

-- Toplevel directives.
type Directive = (Directive', Location)

data Directive' = Help
                | Context
                | Parameter Variable Expr
                | Definition Variable Expr
                | Check Expr
                | Eval Expr
