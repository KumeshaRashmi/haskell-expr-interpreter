# Part D: imperative-style evaluator (Python), for comparison with Haskell.
# Nodes are tuples: ("lit", n), ("var", name), ("add", a, b), ("sub", a, b),
# ("mul", a, b), ("div", a, b).  Mutable dict for variables, if/elif chain,
# and exceptions for errors.

def evaluate(node, env):
    kind = node[0]
    if kind == "lit":
        return node[1]
    elif kind == "var":
        return env[node[1]]            # KeyError if undefined (runtime!)
    else:
        result = evaluate(node[1], env)
        right = evaluate(node[2], env)
        if kind == "add":
            result = result + right
        elif kind == "sub":
            result = result - right
        elif kind == "mul":
            result = result * right
        elif kind == "div":
            if right == 0:
                raise ZeroDivisionError("Division by zero")
            result = result / right
        else:
            raise ValueError("unknown node: " + kind)   # typo -> runtime only
        return result

if __name__ == "__main__":
    env = {"x": 5, "y": 2}
    print(evaluate(("add", ("lit", 2), ("mul", ("lit", 3), ("lit", 4))), env))  # 14
    try:
        print(evaluate(("add", ("var", "x"), ("var", "w")), env))
    except KeyError as e:
        print("Error: undefined variable", e)
