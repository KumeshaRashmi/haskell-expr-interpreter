module Expr
  ( Expr(..)
  , Env
  , eval
  , simplify
  , evalBatch
  , batchReport
  , pretty
  ) where

import Data.Either (rights)

--------------------------------------------------------------------
-- Part A: Language design
--------------------------------------------------------------------

-- | The expression language. 'Let' is the extension constructor:
--   @Let "x" e1 e2@ means  let x = e1 in e2.
data Expr
  = Lit Double            -- ^ numeric literal
  | Var String            -- ^ variable reference
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Let String Expr Expr  -- ^ local binding
  deriving (Show, Eq)

-- | Variable bindings: an association list of names to values.
type Env = [(String, Double)]

--------------------------------------------------------------------
-- Part B: Evaluation
--------------------------------------------------------------------

-- | Evaluate an expression in an environment.
--   Failures are reported with 'Left'; no runtime exceptions are used.
--   The Either monad's do-notation propagates the first error found.
eval :: Env -> Expr -> Either String Double
eval _   (Lit n) = Right n
eval env (Var x) =
  case lookup x env of
    Nothing -> Left ("Undefined variable: " ++ x)
    Just v  -> Right v
eval env (Add a b) = do
  x <- eval env a
  y <- eval env b
  return (x + y)
eval env (Sub a b) = do
  x <- eval env a
  y <- eval env b
  return (x - y)
eval env (Mul a b) = do
  x <- eval env a
  y <- eval env b
  return (x * y)
eval env (Div a b) = do
  x <- eval env a
  y <- eval env b
  if y == 0
    then Left "Division by zero"
    else return (x / y)
eval env (Let name bound body) = do
  v <- eval env bound
  -- A new environment is built; the old one is never modified.
  -- Prepending also gives correct shadowing, since lookup finds
  -- the first match.
  eval ((name, v) : env) body

--------------------------------------------------------------------
-- Part C: Higher-order functions
--------------------------------------------------------------------

-- | Bottom-up algebraic simplification. Children are simplified first,
--   then identities are applied to the simplified children.
--
--   Valid identities: x+0 = 0+x = x ; x-0 = x ; x*1 = 1*x = x ; x/1 = x
--
--   We intentionally omit x*0 = 0*x = 0 because it is not semantics-preserving
--   in the presence of error-producing subexpressions.
simplify :: Expr -> Expr
simplify (Add a b) =
  case (simplify a, simplify b) of
    (Lit 0, e)  -> e
    (e, Lit 0)  -> e
    (x, y)      -> Add x y
simplify (Sub a b) =
  case (simplify a, simplify b) of
    (e, Lit 0)  -> e
    (x, y)      -> Sub x y
simplify (Mul a b) =
  case (simplify a, simplify b) of
    (Lit 1, e)  -> e
    (e, Lit 1)  -> e
    (x, y)      -> Mul x y
simplify (Div a b) =
  case (simplify a, simplify b) of
    (e, Lit 1)  -> e
    (x, y)      -> Div x y
simplify (Let v e body) = Let v (simplify e) (simplify body)
simplify e = e   -- Lit and Var are already simple

-- | Evaluate a batch of expressions, keeping only the successes.
--   Currying: @eval env@ is a partial application of 'eval'.
evalBatch :: Env -> [Expr] -> [Double]
evalBatch env = rights . map (eval env)

-- | Count (successes, failures) over a batch using foldr.
batchReport :: Env -> [Expr] -> (Int, Int)
batchReport env = foldr step (0, 0) . map (eval env)
  where
    step (Right _) (ok, bad) = (ok + 1, bad)
    step (Left  _) (ok, bad) = (ok, bad + 1)

--------------------------------------------------------------------
-- Helper: readable output
--------------------------------------------------------------------

-- | Fully parenthesised pretty printer.
pretty :: Expr -> String
pretty (Lit n)       = show n
pretty (Var x)       = x
pretty (Add a b)     = bin "+" a b
pretty (Sub a b)     = bin "-" a b
pretty (Mul a b)     = bin "*" a b
pretty (Div a b)     = bin "/" a b
pretty (Let v e b)   = "(let " ++ v ++ " = " ++ pretty e ++ " in " ++ pretty b ++ ")"

bin :: String -> Expr -> Expr -> String
bin op a b = "(" ++ pretty a ++ " " ++ op ++ " " ++ pretty b ++ ")"
