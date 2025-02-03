import logging
import string
import sys

from dataclasses import dataclass
from enum import Enum
from typing import (
    Callable,
    Mapping,
    Optional,
    Union,
)
from types import MappingProxyType

### TOKENIZE ###

class CharSet(Enum):
    SPACE = frozenset(string.whitespace)
    LPAREN = frozenset("(")
    RPAREN = frozenset(")")
    INTEGER_START = frozenset(string.digits + "-")
    INTEGER_REST = frozenset(string.digits)
    INTEGER_END = frozenset(string.whitespace + "()")
    SYMBOL_START = frozenset(string.ascii_lowercase)
    SYMBOL_REST = frozenset(string.ascii_lowercase + string.digits + "_")
    SYMBOL_END = frozenset(string.whitespace + "()")

@dataclass(frozen=True)
class Token:
    pass

@dataclass(frozen=True)
class TokenLParen(Token):
    pass

@dataclass(frozen=True)
class TokenRParen(Token):
    pass

@dataclass(frozen=True)
class TokenNonParen(Token):
    pass

@dataclass(frozen=True)
class TokenInteger(TokenNonParen):
    value: int

@dataclass(frozen=True)
class TokenSymbol(TokenNonParen):
    """
    A symbol is a sequence of characters that can represent many things:
    - The name of an identifier
    - The name of a built-in operator
    - The textual representation of a literal
    - A reserved keyword
    """
    value: str

def tokenize(source_code: str) -> list[Token]:
    tokens, _ = tokenize_helper(source_code, [])
    return tokens

def tokenize_helper(source_code: str, tokens: list[Token]) -> tuple[list[Token], str]:
    if not source_code:
        return tokens, source_code
    ch, rest = source_code[0], source_code[1:]
    if ch in CharSet.SPACE.value:
        return tokenize_helper(rest, tokens)
    elif ch in CharSet.LPAREN.value:
        return tokenize_helper(rest, tokens + [TokenLParen()])
    elif ch in CharSet.RPAREN.value:
        return tokenize_helper(rest, tokens + [TokenRParen()])
    elif ch in CharSet.INTEGER_START.value:
        consumed, remaining = consume_until_delimiter(source_code, CharSet.INTEGER_END.value)
        if is_valid_integer(consumed):
            return tokenize_helper(remaining, tokens + [TokenInteger(value=int(consumed))])
        else:
            raise RuntimeError(f"Invalid integer '{consumed}'")
    elif ch in CharSet.SYMBOL_START.value:
        consumed, remaining = consume_until_delimiter(source_code, CharSet.SYMBOL_END.value)
        if is_valid_symbol(consumed):
            return tokenize_helper(remaining, tokens + [TokenSymbol(value=consumed)])
        else:
            raise RuntimeError(f"Invalid symbol '{consumed}'")
    else:
        raise RuntimeError(f"Unexpected character '{ch}'")

def consume_until_delimiter(source_code: str, end_delimiters: set[str]) -> tuple[str, str]:
    generator = (i for i, ch in enumerate(source_code) if ch in end_delimiters)
    end = next(generator, len(source_code))
    return source_code[:end], source_code[end:]

def is_valid_integer(text: str) -> bool:
    return text and text[0] in CharSet.INTEGER_START.value and all(c in CharSet.INTEGER_REST.value for c in text[1:])

def is_valid_symbol(text: str) -> bool:
    return text and text[0] in CharSet.SYMBOL_START.value and all(c in CharSet.SYMBOL_REST.value for c in text[1:])

### PARSE ###

class Keyword(Enum):
    TRUE = "true"
    FALSE = "false"
    NONE = "none"
    LAMBDA = "lambda"
    IF = "if"

class Expr:
    pass

@dataclass(frozen=True)
class Integer(Expr):
    value: int

@dataclass(frozen=True)
class Symbol(Expr):
    value: str

@dataclass(frozen=True)
class Lambda(Expr):
    parameters: list[str]
    body: Expr

@dataclass(frozen=True)
class Call(Expr):
    operator: Expr
    arguments: list[Expr]

@dataclass(frozen=True)
class If(Expr):
    condition: Expr
    consequent: Expr
    alternative: Expr

TokenNode = Union[TokenNonParen, list["TokenNode"]]

def parse(tokens: list[Token]) -> Expr:
    token_node, _ = parse_parens(tokens)
    expr, _ = parse_expr(token_node)
    return expr

def parse_parens(tokens: list[Token]) -> tuple[TokenNode, list[Token]]:
    if not tokens:
        raise RuntimeError("Unexpected end of tokens (when parsing parentheses)")
    token, *rest = tokens
    if isinstance(token, (TokenInteger, TokenSymbol)):
        return token, rest
    elif isinstance(token, TokenLParen):
        return parse_parens_group(rest, [])
    else:
        raise RuntimeError(f"Unexpected token (when parsing parentheses): {token}")

def parse_parens_group(remaining: list[Token], group: TokenNode) -> tuple[TokenNode, list[Token]]:
    if not remaining:
        raise RuntimeError("Unexpected end of tokens (when parsing parentheses group)")
    token, *rest = remaining
    if isinstance(token, TokenRParen):
        return group, rest
    token_node, new_remaining = parse_parens(remaining)
    return parse_parens_group(new_remaining, group + [token_node])

def parse_expr(t: TokenNode) -> tuple[Expr, TokenNode]:
    if isinstance(t, TokenInteger):
        return Integer(value=int(t.value)), []
    elif isinstance(t, TokenSymbol):
        return Symbol(t.value), []
    elif isinstance(t, list):
        operator = t[0]
        if operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.LAMBDA.value:
            return parse_lambda(t)
        elif operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.IF.value:
            return parse_if(t)
        else:
            return parse_call(t)
    else:
        raise RuntimeError(f"Unexpected token (when parsing expression): {t}")

def parse_lambda(t: TokenNode) -> tuple[Lambda, TokenNode]:
    if len(t) != 3:
        raise RuntimeError("Malformed lambda expression")
    _, parameters, body, *rest = t
    return Lambda(
        parameters=list(map(parse_closure_parameter, parameters)),
        body=parse_expr(body)[0],
    ), rest

def parse_closure_parameter(t: TokenNode) -> str:
    if isinstance(t, TokenSymbol):
        return t.value
    else:
        raise RuntimeError(f"Unexpected token in parameter list: {t}")

def parse_if(t: TokenNode) -> tuple[If, TokenNode]:
    if len(t) != 4:
        raise RuntimeError("Malformed if expression")
    _, condition, consequent, alternative, *rest = t
    return If(
        condition=parse_expr(condition)[0], 
        consequent=parse_expr(consequent)[0], 
        alternative=parse_expr(alternative)[0]
    ), rest

def parse_call(t: TokenNode) -> tuple[Call, TokenNode]:
    if len(t) == 0:
        raise RuntimeError("Malformed call expression")
    operator, *rest = t
    return Call(
        operator=parse_expr(operator)[0],
        arguments=list(map(lambda t: parse_expr(t)[0], rest))
    ), rest

### EVAL ###

Value = Union[int, bool, Callable, "Closure", None]

@dataclass(frozen=True)
class Closure:
    parameters: list[str]
    body: Expr
    env: "Frame"

@dataclass(frozen=True)
class Frame:
    bindings: Mapping[str, Value]
    parent: Optional["Frame"]

    def get(self, name: str) -> Optional[Value]:
        if name in self.bindings:
            return self.bindings[name]
        elif self.parent:
            return self.parent.get(name)
        else:
            return None

BUILTINS: Mapping[str, Callable] = MappingProxyType({
    "add": lambda a, b: a + b,
    "sub": lambda a, b: a - b,
    "mul": lambda a, b: a * b,
    "div": lambda a, b: a // b,
    "eq": lambda a, b: a == b,
    "lt": lambda a, b: a < b,
    "gt": lambda a, b: a > b,
    "leq": lambda a, b: a <= b,
    "geq": lambda a, b: a >= b,
    "pair": lambda a, b: (a, b),
})

def eval(expr: Expr) -> Value:
    env = base_env()
    return eval_expr(expr, env)

def base_env() -> Frame:
    return Frame(bindings=BUILTINS, parent=None)

def eval_expr(expr: Expr, env: Optional[Frame] = None) -> Value:
    if isinstance(expr, Integer):
        return int(expr.value)
    elif isinstance(expr, Symbol):
        if expr.value == Keyword.TRUE.value:
            return True
        elif expr.value == Keyword.FALSE.value:
            return False
        elif expr.value == Keyword.NONE.value:
            return None
        if env is None:
            raise RuntimeError(f"Undefined identifier: {expr.value}")
        value = env.get(expr.value)
        if value is None:
            raise RuntimeError(f"Undefined identifier: {expr.value}")
        return value
    elif isinstance(expr, Lambda):
        return Closure(parameters=expr.parameters, body=expr.body, env=env)
    elif isinstance(expr, If):
        condition = eval_expr(expr.condition, env)
        if condition:
            return eval_expr(expr.consequent, env)
        else:
            return eval_expr(expr.alternative, env)
    elif isinstance(expr, Call):
        return eval_call(expr, env)
    else:
        raise RuntimeError(f"Unsupported expression: {expr}")

def eval_call(expr: Call, env: Frame) -> Value:
    operator = eval_expr(expr.operator, env)
    if isinstance(operator, Callable):
        arguments = [eval_expr(arg, env) for arg in expr.arguments]
        return operator(*arguments)
    elif isinstance(operator, Closure):
        if len(operator.parameters) != len(expr.arguments):
            raise RuntimeError("Argument count mismatch")
        bindings = MappingProxyType({
            param: eval_expr(arg, env) for param, arg in zip(operator.parameters, expr.arguments)
        })
        child_env = Frame(
            bindings=bindings,
            parent=operator.env
        )
        return eval_expr(operator.body, child_env)
    else:
        raise RuntimeError("Call expression must have a callable operator")

### MAIN ###

def main():
    logging.basicConfig(level=logging.DEBUG, format="[%(levelname)s] %(message)s")
    source_file_path = sys.argv[1] if len(sys.argv) == 2 else None
    if not source_file_path:
        logging.error("Usage: python prim.py <FILE_PATH>")
        sys.exit(1)
    try:
        with open(source_file_path, "r", encoding="utf-8") as source_file:
            source_code = source_file.read()
            tokens = tokenize(source_code)
            logging.debug(f"Tokenize result: {tokens}")
            if tokens:
                expr = parse(tokens)
                logging.debug(f"Parse result: {expr}")
            if expr:
                value = eval(expr)
                logging.debug(f"Evaluation result: {value}")
    except FileNotFoundError:
        logging.error(f"File not found: {source_file_path}")
        sys.exit(1)

if __name__ == "__main__":
    main()
