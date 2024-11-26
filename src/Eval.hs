{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Eval where

import Syntax (Expr (..), Literal (..), Program (..), primitiveFunctionSymbols)
import qualified Data.Map as Map

type Env = Map.Map String Value

data Value
    = LitVal Literal
    | FuncVal [String] Expr Env -- parameter list, function body and closure
    deriving (Show, Eq, Ord)

type Eval a = Either String a

evalPrimitive :: String -> [Value] -> Eval Value
evalPrimitive op args = case op of
    -- Arithmetic Operations
    "+" -> numOp (+) args
    "-" -> numOp (-) args
    "*" -> numOp (*) args
    "/" -> numOp safeDiv args
    -- Numerical Comparisons
    "=" -> numCmp (==) args
    "<" -> numCmp (<) args
    ">" -> numCmp (>) args
    "<=" -> numCmp (<=) args
    ">=" -> numCmp (>=) args
    -- Logical Operations
    "and" -> boolOp (&&) args
    "or" -> boolOp (||) args
    "not" -> case args of
        [LitVal (Boolean b)] -> Right $ LitVal $ Boolean $ not b
        _ -> Left "Not failed"
    -- Type Predicates
    "number?" -> case args of
        [LitVal (Number _)] -> Right $ LitVal $ Boolean True
        _ -> Right $ LitVal $ Boolean False
    "boolean?" -> case args of
        [LitVal (Boolean _)] -> Right $ LitVal $ Boolean True
        _ -> Right $ LitVal $ Boolean False
    "string?" -> case args of
        [LitVal (StringLiteral _)] -> Right $ LitVal $ Boolean True
        _ -> Right $ LitVal $ Boolean False
    _ -> Left "The argument is not a primitive function"

-- Helper Functions
numOp :: (Double -> Double -> Double) -> [Value] -> Eval Value
numOp f vs = do
    nums <- mapM extractNumber vs
    case nums of
        [] -> Left "No nums"
        (n : ns) -> Right . LitVal . Number $ foldl f n ns

numCmp :: (Double -> Double -> Bool) -> [Value] -> Eval Value
numCmp f [v1, v2] = do
    n1 <- extractNumber v1
    n2 <- extractNumber v2
    return $ LitVal $ Boolean $ f n1 n2
numCmp _ v = Left $ show v ++ ": numCmp failed"

boolOp :: (Bool -> Bool -> Bool) -> [Value] -> Eval Value
boolOp f vs = do
    bools <- mapM extractBoolean vs
    case bools of
        [] -> Left "No bools"
        (b : bs) -> Right $ LitVal $ Boolean $ foldl f b bs

safeDiv :: Double -> Double -> Double
safeDiv _ 0 = error "Division by zero"
safeDiv x y = x / y

extractNumber :: Value -> Eval Double
extractNumber (LitVal (Number n)) = Right n
extractNumber v = Left $ show v ++ ": extractNumber failed"

extractBoolean :: Value -> Eval Bool
extractBoolean (LitVal (Boolean b)) = Right b
extractBoolean v = Left $ show v ++ ": extractBoolean failed"

toEither :: String -> Maybe a -> Either String a
toEither string maybeA = case maybeA of
    Just x -> Right x
    _ -> Left string

evalExpr :: Env -> Expr -> Eval Value
evalExpr env (Literal b) = Right $ LitVal b
evalExpr env (Var x) = toEither "No var found in env" $ Map.lookup x env
evalExpr env (If predicate thenExpr elseExpr) = do
    predVal <- evalExpr env predicate
    case predVal of
        LitVal (Boolean True) -> evalExpr env thenExpr
        LitVal (Boolean False) -> case elseExpr of
            Just elseE -> evalExpr env elseE
            Nothing -> Right $ LitVal $ Boolean False
        _ -> Left "Predicate does not compute to bool"
evalExpr env (ProcedureCall proc args) = do
    argVals <- mapM (evalExpr env) args
    if proc `elem` primitiveFunctionSymbols
        then evalPrimitive proc argVals
        else do
            func <- toEither ("Func name not found: " ++ proc ++ " in " ++ show env) $ Map.lookup proc env
            case func of
                FuncVal params body closureEnv ->
                    if length params == length argVals
                        then
                            let newEnv = Map.unions [Map.fromList $ zip params argVals , closureEnv, env]
                            in evalExpr newEnv body
                        else Left "Params size mismatch"
                _ -> Left "Not a func"
evalExpr env (Define var expr) = do
    val <- evalExpr env expr
    Right val -- Placeholder: Would need to update the environment
evalExpr env (DefineProc name params body) =
    Right $ FuncVal params body env -- Placeholder: Would need to update the environment
evalExpr _ _ = Left "Unknown error"
