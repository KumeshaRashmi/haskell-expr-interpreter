# Part D: imperative-style evaluator (Python), for comparison with Haskell.
# Nodes are tuples: ("lit", n), ("var", name), ("add", a, b), ("sub", a, b),
# ("mul", a, b), ("div", a, b).  Mutable dict for variables, if/elif chain,
# and exceptions for errors.


class EvaluationError(Exception):
    """An error encountered while evaluating an expression."""


def evaluate(node, env):
    if not isinstance(node, tuple) or not node:
        raise EvaluationError("Invalid expression node")

    kind = node[0]
    if kind == "lit":
        if len(node) != 2:
            raise EvaluationError("Invalid literal node")
        return node[1]
    if kind == "var":
        if len(node) != 2:
            raise EvaluationError("Invalid variable node")
        name = node[1]
        if name not in env:
            raise EvaluationError("Undefined variable: " + name)
        return env[name]

    if kind not in ("add", "sub", "mul", "div"):
        raise EvaluationError("Unknown node: " + str(kind))
    if len(node) != 3:
        raise EvaluationError("Invalid " + kind + " node")

    left = evaluate(node[1], env)
    right = evaluate(node[2], env)

    if kind == "add":
        return left + right
    if kind == "sub":
        return left - right
    if kind == "mul":
        return left * right
    if right == 0:
        raise EvaluationError("Division by zero")
    return left / right

if __name__ == "__main__":
    env = {"x": 5, "y": 2}
    print(evaluate(("add", ("lit", 2), ("mul", ("lit", 3), ("lit", 4))), env))  # 14
    try:
        print(evaluate(("add", ("var", "x"), ("var", "w")), env))
    except EvaluationError as error:
        print("Error:", error)
