from dataclasses import dataclass
from prim.keyword import Keyword
from prim.lex import (
    Token,
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
    condition: Expr
    consequent: Expr
    alternative: Expr

### PARSING ###

_TokenNode = TokenNonParen | list["_TokenNode"]

def parse(tokens: list[Token]) -> Expr:
    token_node, _ = _parse_parens(tokens)
    expr, _ = _parse_expr(token_node)
    return expr

def _parse_parens(tokens: list[Token]) -> tuple[_TokenNode, list[Token]]:
    if not tokens:
        raise RuntimeError("Unexpected end of tokens (when parsing parentheses)")
    token, *rest = tokens
    if isinstance(token, (TokenInt, TokenSymbol, TokenString)):
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

def _parse_expr(t: _TokenNode) -> tuple[Expr, _TokenNode]:
    if isinstance(t, TokenInt):
        return IntLiteral(value=int(t.value)), []
    elif isinstance(t, TokenSymbol):
        return SymbolLiteral(t.value), []
    elif isinstance(t, TokenString):
        return StringLiteral(t.value), []
    elif isinstance(t, list):
        operator = t[0]
        if operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.LAMBDA.value:
            return _parse_lambda(t)
        elif operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.IF.value:
            return _parse_if(t)
        else:
            return _parse_call(t)
    else:
        raise RuntimeError(f"Unexpected token (when parsing expression): {t}")

def _parse_lambda(t: _TokenNode) -> tuple[LambdaExpr, _TokenNode]:
    if not isinstance(t, list) or len(t) != 3:
        raise RuntimeError("Malformed lambda expression")
    _, params, body, *rest = t
    if not isinstance(params, list):
        raise RuntimeError("Malformed lambda expression parameters")
    return LambdaExpr(
        params=list(map(_parse_closure_param, params)),
        body=_parse_expr(body)[0],
    ), rest

def _parse_closure_param(t: _TokenNode) -> str:
    if isinstance(t, TokenSymbol):
        return t.value
    else:
        raise RuntimeError(f"Unexpected token in parameter list: {t}")

def _parse_if(t: _TokenNode) -> tuple[IfExpr, _TokenNode]:
    if not isinstance(t, list) or len(t) != 4:
        raise RuntimeError("Malformed if expression")
    _, condition, consequent, alternative, *rest = t
    return IfExpr(
        condition=_parse_expr(condition)[0], 
        consequent=_parse_expr(consequent)[0], 
        alternative=_parse_expr(alternative)[0]
    ), rest

def _parse_call(t: _TokenNode) -> tuple[CallExpr, _TokenNode]:
    if not isinstance(t, list) or len(t) == 0:
        raise RuntimeError("Malformed call expression")
    operator, *rest = t
    return CallExpr(
        operator=_parse_expr(operator)[0],
        args=list(map(lambda t: _parse_expr(t)[0], rest))
    ), rest
