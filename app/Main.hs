module Main (main) where

import Expr

env0 :: Env
env0 = [("x", 5), ("y", 2)]

-- (label, expression, expected result as printed by 'show')
samples :: [(String, Expr, String)]
samples =
  [ ("1. Arithmetic",       Add (Lit 2) (Mul (Lit 3) (Lit 4)),                 "Right 14.0")
  , ("2. Variables",        Sub (Var "x") (Var "y"),                           "Right 3.0")
  , ("3. Let binding",      Let "z" (Lit 10) (Div (Var "z") (Var "y")),        "Right 5.0")
  , ("4. Division by zero", Div (Lit 1) (Sub (Var "x") (Lit 5)),               "Left \"Division by zero\"")
  , ("5. Undefined var",    Add (Var "x") (Var "w"),                           "Left \"Undefined variable: w\"")
  , ("6. Let shadowing",    Let "x" (Lit 1) (Add (Var "x") (Var "y")),         "Right 3.0")
  ]

main :: IO ()
main = do
  putStrLn "=== Sample evaluations (env: x=5, y=2) ==="
  mapM_ runSample samples

  putStrLn "\n=== simplify ==="
  let e = Add (Mul (Var "x") (Lit 1)) (Lit 0)
  putStrLn ("before: " ++ pretty e)
  putStrLn ("after : " ++ pretty (simplify e))
  putStrLn ("x*1   : " ++ pretty (simplify (Mul (Var "x") (Lit 1))))
  putStrLn ("(y/1)-0: " ++ pretty (simplify (Sub (Div (Var "y") (Lit 1)) (Lit 0))))
  putStrLn ("bad*0 : " ++ pretty (simplify (Mul (Div (Lit 1) (Lit 0)) (Lit 0))))

  putStrLn "\n=== Batch (list HOFs + currying) ==="
  let exprs = [ e' | (_, e', _) <- samples ]
  putStrLn ("successful values: " ++ show (evalBatch env0 exprs))
  let (ok, bad) = batchReport env0 exprs
  putStrLn ("succeeded: " ++ show ok ++ ", failed: " ++ show bad)

runSample :: (String, Expr, String) -> IO ()
runSample (label, expr, expected) = do
  let actual = show (eval env0 expr)
  putStrLn (label ++ ": " ++ pretty expr)
  putStrLn ("   expected: " ++ expected)
  putStrLn ("   actual  : " ++ actual ++ (if actual == expected then "   [OK]" else "   [MISMATCH]"))
