# EC 8206 - Expression Interpreter (Haskell)
# EG/2021/4748-Rashmi E.D.K

## Structure
    src/Expr.hs               ADT, eval, simplify, batch functions, pretty printer
    app/Main.hs               Sample evaluations (expected vs actual) and demos
    python/eval_imperative.py Imperative eval for Part D comparison

## How to run
    ghc -isrc -Wall -outputdir build -o interpreter app/Main.hs
    ./interpreter

Or interactively:

    ghci -isrc app/Main.hs
    ghci> eval [("x",5)] (Add (Var "x") (Lit 2))

Imperative version: `python3 eval/eval_imperative.py`
