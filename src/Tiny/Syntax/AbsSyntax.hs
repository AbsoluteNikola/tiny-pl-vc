-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Syntax.

module Tiny.Syntax.AbsSyntax where

import Prelude (Integer)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

import qualified Data.Text
import qualified Data.Data    as C (Data, Typeable)
import qualified GHC.Generics as C (Generic)

data Expr
    = ExprVar VarIdent | ExprConst Integer | ExprOp Expr IntOp Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data IntOp = Plus | Minus | Multiply | Div | Mod
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data IntCondOp = Eq | NotEq | Gt | GtEq | Lt | LtEq
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data BoolCondOp = Or | And
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Cond
    = IntCond Expr IntCondOp Expr
    | BoolCond Cond BoolCondOp Cond
    | NotCond Cond
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Annotatation = Annotatation Cond
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Statement
    = Assign VarIdent Expr
    | Composition [Statement]
    | While Annotatation Cond Statement
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

newtype VarIdent = VarIdent Data.Text.Text
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

