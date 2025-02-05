from dataclasses import dataclass
from prim.ast import (
    CallExpr,
    Expr,
    IfExpr,
    IntLiteral,
    LambdaExpr,
    SymbolLiteral,
)
from prim.keyword import Keyword
from types import MappingProxyType
from typing import (
    Callable,
    Mapping,
    Optional,
)

### BUILTINS ###

class Builtin:
    pass

@dataclass(frozen=True)
class BuiltinBinaryMath(Builtin):
    fn: Callable[[int, int], int]

@dataclass(frozen=True)
class BuiltinBinaryPredicate(Builtin):
    fn: Callable[[int, int], bool]

BUILTINS: Mapping[str, Builtin] = MappingProxyType({
    "add": BuiltinBinaryMath(fn=lambda a, b: a + b),
    "sub": BuiltinBinaryMath(fn=lambda a, b: a - b),
    "mul": BuiltinBinaryMath(fn=lambda a, b: a * b),
    "div": BuiltinBinaryMath(fn=lambda a, b: a // b),
    "eq": BuiltinBinaryPredicate(fn=lambda a, b: a == b),
    "lt": BuiltinBinaryPredicate(fn=lambda a, b: a < b),
    "gt": BuiltinBinaryPredicate(fn=lambda a, b: a > b),
    "leq": BuiltinBinaryPredicate(fn=lambda a, b: a <= b),
    "geq": BuiltinBinaryPredicate(fn=lambda a, b: a >= b),
})

### ENVIRONMENT ###

@dataclass(frozen=True)
class Frame:
    bindings: Mapping[str, "Value"]
    parent: Optional["Frame"]

    def get(self, name: str) -> Optional["Value"]:
        if name in self.bindings:
            return self.bindings[name]
        elif self.parent:
            return self.parent.get(name)
        else:
            return None

def base_env() -> Frame:
    return Frame(bindings=BUILTINS, parent=None)

### VALUES ###

@dataclass(frozen=True)
class Closure:
    params: list[str]
    body: Expr
    env: "Frame"

### EVALUATION ###

Value = int | bool | Builtin | Closure

def eval(expr: Expr) -> Value:
    env = base_env()
    return _eval_expr(expr, env)

def _eval_expr(expr: Expr, env: Frame) -> Value:
    if isinstance(expr, IntLiteral):
        return int(expr.value)
    elif isinstance(expr, SymbolLiteral):
        if expr.value == Keyword.TRUE.value:
            return True
        elif expr.value == Keyword.FALSE.value:
            return False
        if env is None:
            raise RuntimeError(f"Undefined identifier: {expr.value}")
        value = env.get(expr.value)
        if value is None:
            raise RuntimeError(f"Undefined identifier: {expr.value}")
        return value
    elif isinstance(expr, LambdaExpr):
        return Closure(params=expr.params, body=expr.body, env=env)
    elif isinstance(expr, IfExpr):
        condition = _eval_expr(expr.condition, env)
        if condition:
            return _eval_expr(expr.consequent, env)
        else:
            return _eval_expr(expr.alternative, env)
    elif isinstance(expr, CallExpr):
        return _eval_call(expr, env)
    else:
        raise RuntimeError(f"Unsupported expression: {expr}")

def _eval_call(expr: CallExpr, env: Frame) -> Value:
    operator = _eval_expr(expr.operator, env)
    args = [_eval_expr(arg, env) for arg in expr.args]
    if isinstance(operator, Builtin):
        if isinstance(operator, (BuiltinBinaryMath, BuiltinBinaryPredicate)):
            if len(args) != 2:
                raise RuntimeError("Expected 2 arguments")
            a, b = args
            if not isinstance(a, int) or not isinstance(b, int):
                raise RuntimeError("Expected 2 integer arguments")
            return operator.fn(a, b)
        raise RuntimeError("Unsupported operator")
    elif isinstance(operator, Closure):
        if len(operator.params) != len(expr.args):
            raise RuntimeError("Argument count mismatch")
        bindings = MappingProxyType({
            param: arg for param, arg in zip(operator.params, args)
        })
        child_env = Frame(
            bindings=bindings,
            parent=operator.env
        )
        return _eval_expr(operator.body, child_env)
    else:
        raise RuntimeError("Unsupported operator")
