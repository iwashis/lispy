{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib where

import Eval
import Parser
import Syntax
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.Map as Map

exprs :: [String]
exprs =
    [ "(defun factorial (x) (if (= x 0) 1 (* x (factorial (- x 1)))))"
    , "(defun fib (x) (if (= x 0) 0 (if (= x 1) 1 (+ (fib (- x 1)) (fib (- x 2))))))"
    ]

vals = do
    expr <- exprs
    let (Right func) = parse exprParser "" expr
    let DefineProc name params body = func
    let env = Map.fromList [(name, FuncVal params body Map.empty)]
    let Right e = parse exprParser "" $ "(" ++ name ++ " 5)"
    pure $ evalExpr env e

someFunc :: IO ()
someFunc = print vals
