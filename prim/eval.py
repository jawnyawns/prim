from dataclasses import dataclass
from prim.ast import (
    CallExpr,
    Expr,
    FloatLiteral,
    IfExpr,
    IntLiteral,
    LambdaExpr,
    SymbolLiteral,
    StringLiteral,
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
class IntIntToInt(Builtin):
    fn: Callable[[int, int], int]

@dataclass(frozen=True)
class IntIntToBool(Builtin):
    fn: Callable[[int, int], bool]

@dataclass(frozen=True)
class BoolBoolToBool(Builtin):
    fn: Callable[[bool, bool], bool]

@dataclass(frozen=True)
class BoolToBool(Builtin):
    fn: Callable[[bool], bool]

BUILTINS: Mapping[str, Builtin] = MappingProxyType({
    "add": IntIntToInt(fn=lambda a, b: a + b),
    "sub": IntIntToInt(fn=lambda a, b: a - b),
    "mul": IntIntToInt(fn=lambda a, b: a * b),
    "div": IntIntToInt(fn=lambda a, b: a // b),
    "eq": IntIntToBool(fn=lambda a, b: a == b),
    "lt": IntIntToBool(fn=lambda a, b: a < b),
    "gt": IntIntToBool(fn=lambda a, b: a > b),
    "leq": IntIntToBool(fn=lambda a, b: a <= b),
    "geq": IntIntToBool(fn=lambda a, b: a >= b),
    "and": BoolBoolToBool(fn=lambda a, b: bool(a and b)),
    "or": BoolBoolToBool(fn=lambda a, b: bool(a or b)),
    "not": BoolToBool(fn=lambda a: not a),
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
    env: Frame

Value = int | float | bool | str | Builtin | Closure

### EVALUATION ###

def eval(expr: Expr) -> Value:
    env = base_env()
    return _eval_expr(expr, env)

def _eval_expr(expr: Expr, env: Frame) -> Value:
    if isinstance(expr, IntLiteral):
        return expr.value
    elif isinstance(expr, FloatLiteral):
        return expr.value
    elif isinstance(expr, SymbolLiteral):
        return _eval_symbol(expr, env)
    elif isinstance(expr, StringLiteral):
        return str(expr.value)
    elif isinstance(expr, LambdaExpr):
        return Closure(params=expr.params, body=expr.body, env=env)
    elif isinstance(expr, IfExpr):
        return _eval_if(expr, env)
    elif isinstance(expr, CallExpr):
        return _eval_call(expr, env)
    else:
        raise RuntimeError(f"Unsupported expression: {expr}")

def _eval_symbol(expr: SymbolLiteral, env: Frame) -> Value:
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

def _eval_if(expr: IfExpr, env: Frame) -> Value:
    for condition, consequent in zip(expr.conditions, expr.consequents):
        if _eval_expr(condition, env):
            return _eval_expr(consequent, env)
    return _eval_expr(expr.alternative, env)

def _eval_call(expr: CallExpr, env: Frame) -> Value:
    operator = _eval_expr(expr.operator, env)
    args = [_eval_expr(arg, env) for arg in expr.args]
    if isinstance(operator, Builtin):
        return _eval_call_builtin(operator, args)
    elif isinstance(operator, Closure):
        return _eval_call_closure(operator, args)
    else:
        raise RuntimeError(f"Unsupported operator: {operator}")

def _eval_call_builtin(operator: Builtin, args: list[Value]) -> Value:
    if isinstance(operator, (IntIntToInt, IntIntToBool)):
        if len(args) != 2:
            raise RuntimeError("Expected 2 arguments")
        a, b = args
        if not isinstance(a, int) or not isinstance(b, int):
            raise RuntimeError("Expected 2 integer arguments")
        return operator.fn(a, b)
    elif isinstance(operator, BoolBoolToBool):
        if len(args) != 2:
            raise RuntimeError("Expected 2 arguments")
        a, b = args
        if not isinstance(a, bool) or not isinstance(b, bool):
            raise RuntimeError("Expected 2 boolean arguments")
        return operator.fn(a, b)
    elif isinstance(operator, BoolToBool):
        if len(args) != 1:
            raise RuntimeError("Expected 1 arguments")
        a, = args
        if not isinstance(a, bool):
            raise RuntimeError("Expected 2 boolean arguments")
        return operator.fn(a)
    raise RuntimeError(f"Unsupported operator: {operator}")

def _eval_call_closure(operator: Closure, args: list[Value]) -> Value:
    if len(args) != len(operator.params):
        raise RuntimeError("Argument count mismatch")
    bindings = MappingProxyType({
        param: arg for param, arg in zip(operator.params, args)
    })
    child_env = Frame(
        bindings=bindings,
        parent=operator.env
    )
    return _eval_expr(operator.body, child_env)
