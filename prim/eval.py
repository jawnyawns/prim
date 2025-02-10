from dataclasses import dataclass
from prim.ast import (
    CallExpr,
    DefineExpr,
    Expr,
    FloatLiteral,
    IfExpr,
    IntLiteral,
    LambdaExpr,
    StringLiteral,
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
class NumberNumberToNumber(Builtin):
    fn: Callable[["Number", "Number"], "Number"]

@dataclass(frozen=True)
class NumberNumberToBool(Builtin):
    fn: Callable[["Number", "Number"], bool]

@dataclass(frozen=True)
class BoolBoolToBool(Builtin):
    fn: Callable[[bool, bool], bool]

@dataclass(frozen=True)
class BoolToBool(Builtin):
    fn: Callable[[bool], bool]

@dataclass(frozen=True)
class StringStringToString(Builtin):
    fn: Callable[[str, str], str]

@dataclass(frozen=True)
class ListConstructor(Builtin):
    fn: Callable[[], "List"]

@dataclass(frozen=True)
class PrependList(Builtin):
    fn: Callable[["Value", "List"], "List"]

@dataclass(frozen=True)
class GetValueFromList(Builtin):
    fn: Callable[["ListNode"], "Value"]

@dataclass(frozen=True)
class GetRestFromList(Builtin):
    fn: Callable[["ListNode"], "List"]

BUILTINS: Mapping[str, Builtin] = MappingProxyType({
    "+": NumberNumberToNumber(fn=lambda a, b: a + b),
    "-": NumberNumberToNumber(fn=lambda a, b: a - b),
    "*": NumberNumberToNumber(fn=lambda a, b: a * b),
    "/": NumberNumberToNumber(fn=lambda a, b: a / b),
    "=": NumberNumberToBool(fn=lambda a, b: a == b),
    "<": NumberNumberToBool(fn=lambda a, b: a < b),
    ">": NumberNumberToBool(fn=lambda a, b: a > b),
    "<=": NumberNumberToBool(fn=lambda a, b: a <= b),
    ">=": NumberNumberToBool(fn=lambda a, b: a >= b),
    "and": BoolBoolToBool(fn=lambda a, b: bool(a and b)),
    "or": BoolBoolToBool(fn=lambda a, b: bool(a or b)),
    "not": BoolToBool(fn=lambda a: not a),
    "++": StringStringToString(fn=lambda a, b: a + b),
    "list": ListConstructor(fn=lambda: ListEmpty()),
    "::": PrependList(fn=lambda value, list: ListNode(value=value, rest=list)),
    "value": GetValueFromList(fn=lambda list: list.value),
    "rest": GetRestFromList(fn=lambda list: list.rest),
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

class List:
    pass

@dataclass
class ListEmpty(List):
    pass

@dataclass(frozen=True)
class ListNode(List):
    value: "Value"
    rest: List

Number = int | float
Value = Number | bool | str | Builtin | Closure | List

### EVALUATION ###

def eval(exprs: list[Expr]) -> list[Value]:
    env = base_env()
    return _eval_exprs(exprs, env, [])

def _eval_exprs(exprs: list[Expr], env: Frame, values_acc: list[Value]) -> list[Value]:
    if not exprs:
        return values_acc
    expr, *rest = exprs
    value, new_env = _eval_expr(expr, env)
    return _eval_exprs(rest, new_env, values_acc + [value])

def _eval_expr(expr: Expr, env: Frame) -> tuple[Value, Frame]:
    if isinstance(expr, IntLiteral):
        return expr.value, env
    elif isinstance(expr, FloatLiteral):
        return expr.value, env
    elif isinstance(expr, SymbolLiteral):
        return _eval_symbol(expr, env)
    elif isinstance(expr, StringLiteral):
        return str(expr.value), env
    elif isinstance(expr, LambdaExpr):
        return Closure(params=expr.params, body=expr.body, env=env), env
    elif isinstance(expr, IfExpr):
        return _eval_if(expr, env)
    elif isinstance(expr, CallExpr):
        return _eval_call(expr, env)
    elif isinstance(expr, DefineExpr):
        return _eval_define(expr, env)
    else:
        raise RuntimeError(f"Unsupported expression: {expr}")

def _eval_symbol(expr: SymbolLiteral, env: Frame) -> tuple[Value, Frame]:
    if expr.value == Keyword.TRUE.value:
        return True, env
    elif expr.value == Keyword.FALSE.value:
        return False, env
    value = env.get(expr.value)
    if value is None:
        raise RuntimeError(f"Undefined identifier: {expr.value}")
    return value, env

def _eval_if(expr: IfExpr, env: Frame) -> tuple[Value, Frame]:
    for condition, consequent in zip(expr.conditions, expr.consequents):
        condition_value, _ = _eval_expr(condition, env)
        if not isinstance(condition_value, bool):
            raise RuntimeError("If condition must be a boolean expression")
        if condition_value:
            return _eval_expr(consequent, env)
    return _eval_expr(expr.alternative, env)

def _eval_call(expr: CallExpr, env: Frame) -> tuple[Value, Frame]:
    operator, _ = _eval_expr(expr.operator, env)
    args = [_eval_expr(arg, env)[0] for arg in expr.args]
    if isinstance(operator, Builtin):
        return _eval_call_builtin(operator, args), env
    elif isinstance(operator, Closure):
        return _eval_call_closure(operator, args)
    else:
        raise RuntimeError(f"Unsupported operator: {operator}")

def _eval_call_builtin(operator: Builtin, args: list[Value]) -> Value:
    if isinstance(operator, (NumberNumberToNumber, NumberNumberToBool)):
        if len(args) != 2:
            raise RuntimeError("Expected 2 arguments")
        a, b = args
        if (
            (not isinstance(a, int) and not isinstance(a, float)) or
            (not isinstance(b, int) and not isinstance(b, float))
        ):
            raise RuntimeError("Expected 2 number arguments")
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
    elif isinstance(operator, StringStringToString):
        if len(args) != 2:
            raise RuntimeError("Expected 2 arguments")
        a, b = args
        if not isinstance(a, str) or not isinstance(b, str):
            raise RuntimeError("Expected 2 string arguments")
        return operator.fn(a, b)
    elif isinstance(operator, ListConstructor):
        if len(args) != 0:
            raise RuntimeError("Expected 0 arguments")
        return operator.fn()
    elif isinstance(operator, PrependList):
        if len(args) != 2:
            raise RuntimeError("Expected 2 arguments")
        value, list = args
        if not isinstance(value, Value) or not isinstance(list, List):
            raise RuntimeError("Expected 2 arguments: a value, a list")
        return operator.fn(value, list)
    elif isinstance(operator, GetValueFromList):
        if len(args) != 1:
            raise RuntimeError("Expected 1 arguments")
        list, = args
        if not isinstance(list, ListNode):
            raise RuntimeError("Expected 1 argument: a non-empty list")
        return operator.fn(list)
    elif isinstance(operator, GetRestFromList):
        if len(args) != 1:
            raise RuntimeError("Expected 1 arguments")
        list, = args
        if not isinstance(list, ListNode):
            raise RuntimeError("Expected 1 argument: a non-empty list")
        return operator.fn(list)
    raise RuntimeError(f"Unsupported operator: {operator}")

def _eval_call_closure(operator: Closure, args: list[Value]) -> tuple[Value, Frame]:
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

def _eval_define(expr: DefineExpr, env: Frame) -> tuple[Value, Frame]:
    return "<ENV MODIFIED>", Frame(
        bindings=MappingProxyType({
            **env.bindings,
            expr.name: _eval_expr(expr.body, env)[0]
        }),
        parent=env.parent
    )
