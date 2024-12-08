{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Tiny.Semant where

import Tiny.Syntax.AbsSyntax
import Prelude hiding (fail)
import Control.Monad.Except (ExceptT, MonadIO, MonadError (throwError), runExceptT)
import Tiny.Syntax.PrintSyntax (printTree)
import Data.Foldable (for_)

newtype CheckerM a = CheckerM { runCheckerM :: ExceptT String IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError String)

type Checker = CheckerM Cond

failure :: Show x => x -> CheckerM a
failure x = throwError $ "unimplemented case: " <> show x

substituteExpr :: VarIdent -> Expr -> Expr -> Expr
substituteExpr varId substE expr = case expr of
  ExprVar exprVarId
    | exprVarId == varId -> substE
    | otherwise -> expr
  ExprConst _ -> expr
  ExprOp expr1 intop expr2 ->
    ExprOp
      (substituteExpr varId substE expr1)
      intop
      (substituteExpr varId substE expr2)

substituteExprToCond :: VarIdent -> Expr -> Cond -> Cond
substituteExprToCond varId substE x = case x of
  IntCond expr1 intcondop expr2 ->
    IntCond
      (substituteExpr varId substE expr1)
      intcondop
      (substituteExpr varId substE expr2)
  BoolCond cond1 boolcondop cond2 ->
    BoolCond
      (substituteExprToCond varId substE cond1)
      boolcondop
      (substituteExprToCond varId substE cond2)
  NotCond cond -> NotCond $ substituteExprToCond varId substE cond

genProgramVerificationConditions :: Program -> CheckerM [Cond]
genProgramVerificationConditions (Program (Annotation preCond) statements (Annotation postCond)) = do
  progVc <- genVerificationCondition postCond (Composition statements)
  programAnnotation <- genAnnotateCondition postCond (Composition statements)
  let implCond = BoolCond preCond Implication programAnnotation
  pure $ implCond : progVc

genAnnotateCondition :: Cond -> Statement -> CheckerM Cond
genAnnotateCondition postCond = \case
  Assign varident expr -> pure $ substituteExprToCond varident expr postCond
  Composition statements -> case statements of
    [] -> failure @String "Empty composition! Should be impossible after parsing"
    [s] -> genAnnotateCondition postCond s
    s1:otherStatements -> do
      betaCond <- genAnnotateCondition postCond (Composition otherStatements)
      genAnnotateCondition betaCond s1
  If ifCond thenS elseS -> do
    thenCond <- genAnnotateCondition postCond thenS
    elseCond <- genAnnotateCondition postCond elseS
    pure $ BoolCond
      (BoolCond ifCond And thenCond)
      Or
      (BoolCond (NotCond ifCond) And elseCond)
  While (Annotation annotation) _cond _statement -> pure annotation

genVerificationCondition :: Cond -> Statement -> CheckerM [Cond]
genVerificationCondition postCond x = case x of
  Assign _varident _expr -> pure []
  Composition statements  -> case statements of
    [] -> failure @String "Empty composition! Should be impossible after parsing"
    [s] -> genVerificationCondition postCond s
    s1:otherStatements -> do
      otherStatementsCondtions <- genVerificationCondition postCond (Composition otherStatements)
      otherStatementsAnnotation <- genAnnotateCondition postCond (Composition otherStatements)
      s1Conditions <- genVerificationCondition otherStatementsAnnotation s1
      pure $ otherStatementsCondtions ++ s1Conditions
  If _ifCond thenS elseS -> do
    thenConds <- genVerificationCondition postCond thenS
    elseConds <- genVerificationCondition postCond elseS
    pure $ thenConds ++ elseConds
  While (Annotation annotation) whileCond doS -> do
    doConds <- genVerificationCondition annotation doS
    doAnnotation <- genAnnotateCondition annotation doS
    let
      whileExitCond =
        BoolCond
          (BoolCond annotation And (NotCond whileCond))
          Implication
          postCond
      whileEnterCond =
        BoolCond
          (BoolCond annotation And whileCond)
          Implication
          doAnnotation
    pure $ doConds ++ [whileExitCond, whileEnterCond]

runProgram :: Program -> IO ()
runProgram p = do
  eConds <- runExceptT . runCheckerM $ genProgramVerificationConditions p
  case eConds of
    Left err -> do
      putStrLn "Error:"
      putStrLn err
    Right conds -> do
      for_ (zip [(1 :: Int)..] conds) $ \(ix, cond) -> do
        putStr $ show ix <> ": "
        putStrLn $ printTree cond
