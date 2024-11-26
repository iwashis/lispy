{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Syntax where

newtype Program = Program [Expr]
    deriving (Show, Eq)

data Expr
    = Literal Literal -- A literal value (number, boolean, string, etc.)
    | Var String -- A variable
    | ProcedureCall String [Expr] -- A procedure call: (operator operands...)
    | Lambda [String] Expr -- A lambda expression: (lambda (params) body...)
    | If Expr Expr (Maybe Expr) -- An if expression: (if predicate thenExpr elseExpr)
    | Define String Expr -- A variable definition: (define var expr)
    | DefineProc String [String] Expr -- A procedure definition: (define (name params) body...)
    deriving (Show, Eq, Ord)

data Literal
    = Number Double
    | Boolean Bool
    | StringLiteral String
    deriving (Show, Eq, Ord)

primitiveFunctionSymbols :: [String]
primitiveFunctionSymbols =
    [ "+"
    , "-"
    , "*"
    , "="
    , "<"
    , ">"
    , "<="
    , ">="
    , "and"
    , "or"
    , "not"
    , -- , "car"
      -- , "cdr"
      -- , "list"
      -- , "null?"
      -- , "pair?"
      "number?"
    , "boolean?"
    , "string?"
    ]
