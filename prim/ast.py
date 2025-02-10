from dataclasses import dataclass
from prim.keyword import Keyword
from prim.lex import (
    Token,
    TokenFloat,
    TokenInt,
    TokenLParen,
    TokenNonParen,
    TokenRParen,
    TokenSymbol,
    TokenString,
)

### AST ###

class Expr:
    pass

@dataclass(frozen=True)
class IntLiteral(Expr):
    value: int

@dataclass(frozen=True)
class FloatLiteral(Expr):
    value: float

@dataclass(frozen=True)
class SymbolLiteral(Expr):
    value: str

@dataclass(frozen=True)
class StringLiteral(Expr):
    value: str

@dataclass(frozen=True)
class LambdaExpr(Expr):
    params: list[str]
    body: Expr

@dataclass(frozen=True)
class CallExpr(Expr):
    operator: Expr
    args: list[Expr]

@dataclass(frozen=True)
class IfExpr(Expr):
    conditions: list[Expr]
    consequents: list[Expr]
    alternative: Expr

@dataclass(frozen=True)
class DefineExpr(Expr):
    name: str
    body: Expr

### PARSING ###

_TokenNode = TokenNonParen | list["_TokenNode"]

def parse(tokens: list[Token]) -> list[Expr]:
    return _parse_exprs(tokens, [])

def _parse_exprs(tokens: list[Token], acc: list[Expr]) -> list[Expr]:
    if not tokens:
        return acc
    else:
        token_node, remaining_tokens = _parse_parens(tokens)
        expr = _parse_expr(token_node)
        return _parse_exprs(remaining_tokens, acc + [expr])

def _parse_parens(tokens: list[Token]) -> tuple[_TokenNode, list[Token]]:
    if not tokens:
        raise RuntimeError("Unexpected end of tokens (when parsing parentheses)")
    token, *rest = tokens
    if isinstance(token, (TokenInt, TokenFloat, TokenSymbol, TokenString)):
        return token, rest
    elif isinstance(token, TokenLParen):
        return _parse_parens_group(rest, [])
    else:
        raise RuntimeError(f"Unexpected token (when parsing parentheses): {token}")

def _parse_parens_group(remaining: list[Token], group: _TokenNode) -> tuple[_TokenNode, list[Token]]:
    if not remaining:
        raise RuntimeError("Unexpected end of tokens (when parsing parentheses group)")
    token, *rest = remaining
    if isinstance(token, TokenRParen):
        return group, rest
    token_node, new_remaining = _parse_parens(remaining)
    if not isinstance(group, list):
        raise RuntimeError(f"Group was not a list: {group}")
    return _parse_parens_group(new_remaining, group + [token_node])

def _parse_expr(t: _TokenNode) -> Expr:
    if isinstance(t, TokenInt):
        return IntLiteral(value=t.value)
    if isinstance(t, TokenFloat):
        return FloatLiteral(value=t.value)
    elif isinstance(t, TokenSymbol):
        return SymbolLiteral(t.value)
    elif isinstance(t, TokenString):
        return StringLiteral(t.value)
    elif isinstance(t, list):
        operator = t[0]
        if operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.LAMBDA.value:
            return _parse_lambda(t)
        elif operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.IF.value:
            return _parse_if(t)
        elif operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.DEFINE.value:
            return _parse_define(t)
        else:
            return _parse_call(t)
    else:
        raise RuntimeError(f"Unexpected token (when parsing expression): {t}")

def _parse_lambda(t: _TokenNode) -> LambdaExpr:
    if not isinstance(t, list) or len(t) != 3:
        raise RuntimeError("Malformed lambda expression")
    _, params, body = t
    if not isinstance(params, list):
        raise RuntimeError("Malformed lambda expression parameters")
    return LambdaExpr(
        params=list(map(_parse_closure_param, params)),
        body=_parse_expr(body),
    )

def _parse_closure_param(t: _TokenNode) -> str:
    if isinstance(t, TokenSymbol):
        return t.value
    else:
        raise RuntimeError(f"Unexpected token in parameter list: {t}")

def _parse_if(t: _TokenNode) -> IfExpr:
    if not isinstance(t, list) or len(t) < 4 or len(t) % 2 == 1:
        raise RuntimeError("Malformed if expression", )
    _, *rest = t
    conditions = rest[:-1][::2]
    consequents = rest[1::2]
    default = rest[-1]
    return IfExpr(
        conditions=list(map(lambda t: _parse_expr(t), conditions)),
        consequents=list(map(lambda t: _parse_expr(t), consequents)), 
        alternative=_parse_expr(default)
    )

def _parse_define(t: _TokenNode) -> DefineExpr:
    if not isinstance(t, list) or len(t) != 3:
        raise RuntimeError("Malformed define expression")
    _, name, body = t
    parsed_name = _parse_expr(name)
    if not isinstance(parsed_name, SymbolLiteral):
        raise RuntimeError("Invalid name in definition")
    return DefineExpr(
        name=parsed_name.value,
        body=_parse_expr(body),
    )

def _parse_call(t: _TokenNode) -> CallExpr:
    if not isinstance(t, list) or len(t) == 0:
        raise RuntimeError("Malformed call expression")
    operator, *rest = t
    return CallExpr(
        operator=_parse_expr(operator),
        args=list(map(lambda t: _parse_expr(t), rest))
    )
