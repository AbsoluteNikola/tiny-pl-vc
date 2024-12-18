-- File generated by the BNF Converter (bnfc 2.9.5).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Tiny.Syntax.SkelSyntax where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Tiny.Syntax.AbsSyntax

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transVarIdent :: Tiny.Syntax.AbsSyntax.VarIdent -> Result
transVarIdent x = case x of
  Tiny.Syntax.AbsSyntax.VarIdent string -> failure x

transExpr :: Tiny.Syntax.AbsSyntax.Expr -> Result
transExpr x = case x of
  Tiny.Syntax.AbsSyntax.ExprVar varident -> failure x
  Tiny.Syntax.AbsSyntax.ExprConst integer -> failure x
  Tiny.Syntax.AbsSyntax.ExprOp expr1 intop expr2 -> failure x

transIntOp :: Tiny.Syntax.AbsSyntax.IntOp -> Result
transIntOp x = case x of
  Tiny.Syntax.AbsSyntax.Plus -> failure x
  Tiny.Syntax.AbsSyntax.Minus -> failure x
  Tiny.Syntax.AbsSyntax.Multiply -> failure x
  Tiny.Syntax.AbsSyntax.Div -> failure x
  Tiny.Syntax.AbsSyntax.Mod -> failure x

transIntCondOp :: Tiny.Syntax.AbsSyntax.IntCondOp -> Result
transIntCondOp x = case x of
  Tiny.Syntax.AbsSyntax.Eq -> failure x
  Tiny.Syntax.AbsSyntax.NotEq -> failure x
  Tiny.Syntax.AbsSyntax.Gt -> failure x
  Tiny.Syntax.AbsSyntax.GtEq -> failure x
  Tiny.Syntax.AbsSyntax.Lt -> failure x
  Tiny.Syntax.AbsSyntax.LtEq -> failure x

transBoolCondOp :: Tiny.Syntax.AbsSyntax.BoolCondOp -> Result
transBoolCondOp x = case x of
  Tiny.Syntax.AbsSyntax.Or -> failure x
  Tiny.Syntax.AbsSyntax.And -> failure x
  Tiny.Syntax.AbsSyntax.Implication -> failure x

transCond :: Tiny.Syntax.AbsSyntax.Cond -> Result
transCond x = case x of
  Tiny.Syntax.AbsSyntax.IntCond expr1 intcondop expr2 -> failure x
  Tiny.Syntax.AbsSyntax.BoolCond cond1 boolcondop cond2 -> failure x
  Tiny.Syntax.AbsSyntax.NotCond cond -> failure x

transAnnotation :: Tiny.Syntax.AbsSyntax.Annotation -> Result
transAnnotation x = case x of
  Tiny.Syntax.AbsSyntax.Annotation cond -> failure x

transStatement :: Tiny.Syntax.AbsSyntax.Statement -> Result
transStatement x = case x of
  Tiny.Syntax.AbsSyntax.Assign varident expr -> failure x
  Tiny.Syntax.AbsSyntax.Composition statements -> failure x
  Tiny.Syntax.AbsSyntax.While annotation cond statement -> failure x
  Tiny.Syntax.AbsSyntax.If cond statement1 statement2 -> failure x

transProgram :: Tiny.Syntax.AbsSyntax.Program -> Result
transProgram x = case x of
  Tiny.Syntax.AbsSyntax.Program annotation1 statements annotation2 -> failure x
