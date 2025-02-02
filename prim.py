import logging
import string
import sys

from collections import deque
from dataclasses import dataclass
from enum import Enum
from typing import (
    Callable,
    Deque,
    Optional,
    TypeVar,
    Union,
)

### UTILS ###

T = TypeVar("T")

def peek(l: deque[T]) -> Optional[T]:
    return l[0] if l else None

### TOKENIZE ###

class CharSet(Enum):
    SPACE = set(string.whitespace)
    LPAREN = set("(")
    RPAREN = set(")")
    INTEGER_START = set(string.digits + "-")
    INTEGER_REST = set(string.digits)
    INTEGER_END = set(string.whitespace + "()")
    SYMBOL_START = set(string.ascii_lowercase)
    SYMBOL_REST = set(string.ascii_lowercase + string.digits + "_")
    SYMBOL_END = set(string.whitespace + "()")

@dataclass
class Token:
    pass

@dataclass
class TokenLParen(Token):
    pass

@dataclass
class TokenRParen(Token):
    pass

@dataclass
class TokenNonParen(Token):
    pass

@dataclass
class TokenInteger(TokenNonParen):
    value: int

@dataclass
class TokenSymbol(TokenNonParen):
    """
    A symbol is a sequence of characters that can represent many things:
    - The name of an identifier
    - The name of a built-in operator
    - The textual representation of a literal
    - A reserved keyword
    Merging all these into a single token simplifies lexing and parsing.
    """
    value: str

def tokenize(source_code: str) -> deque[Token]:
    tokens = deque()
    index = 0
    while index < len(source_code):
        character = source_code[index]
        if character in CharSet.SPACE.value:
            index += 1
        elif character in CharSet.LPAREN.value:
            tokens.append(TokenLParen())
            index += 1
        elif character in CharSet.RPAREN.value:
            tokens.append(TokenRParen())
            index += 1
        elif character in CharSet.INTEGER_START.value:
            text, index = consume_until_delimiter(index, source_code, CharSet.INTEGER_END.value)
            if is_valid_integer(text):
                tokens.append(TokenInteger(value=int(text)))
            else:
                raise RuntimeError(f"Invalid integer '{text}' preceding position {index}")
        elif character in CharSet.SYMBOL_START.value:
            text, index = consume_until_delimiter(index, source_code, CharSet.SYMBOL_END.value)
            if is_valid_symbol(text):
                tokens.append(TokenSymbol(value=text))
            else:
                raise RuntimeError(f"Invalid symbol '{text}' preceding position {index}")
        else:
            raise RuntimeError(f"Unexpected character '{character}' at position {index}")
    return tokens

def consume_until_delimiter(start: int, source_code: str, end_delimiters: set[str]) -> tuple[str, int]:
    end = start
    while end < len(source_code) and source_code[end] not in end_delimiters:
        end += 1
    text = source_code[start:end]
    return text, end

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

Value = Union[int, bool, Callable, None]

BUILTINS = {
    "add": lambda a, b: a + b,
    "sub": lambda a, b: a - b,
    "mul": lambda a, b: a * b,
    "div": lambda a, b: a // b,
    "eq": lambda a, b: a == b,
    "lt": lambda a, b: a < b,
    "gt": lambda a, b: a > b,
    "leq": lambda a, b: a <= b,
    "geq": lambda a, b: a >= b,
}

class Frame:
    def __init__(self, parent: Optional["Frame"] = None):
        self.bindings: dict[str, Value] = {}
        self.parent: "Frame" = parent

    def get(self, name: str) -> Optional[Value]:
        if name in self.bindings:
            return self.bindings[name]
        elif self.parent:
            return self.parent.get(name)
        else:
            return None

    def set(self, name: str, value: Value):
        self.bindings[name] = value

class Expression:
    pass

@dataclass
class Integer(Expression):
    value: int

@dataclass
class Symbol(Expression):
    value: str

@dataclass
class Closure(Expression):
    parameters: list[str]
    body: Expression
    environment: Frame

@dataclass
class Invocation(Expression):
    operator: Expression
    arguments: list[Expression]

@dataclass
class If(Expression):
    condition: Expression
    consequent: Expression
    alternative: Expression

TokenNode = Union[TokenNonParen, Deque["TokenNode"]]

def parse(tokens: deque[Token]) -> Expression:
    token_node = parse_parenths(tokens)
    return parse_expression(token_node)

def parse_parenths(tokens: deque[Token]) -> TokenNode:
    if not tokens:
        raise RuntimeError("Unexpected end of tokens (when parsing parentheses)")
    token = tokens.popleft()
    if isinstance(token, (TokenInteger, TokenSymbol)):
        return token
    elif isinstance(token, TokenLParen):
        group = deque()
        while tokens:
            next_token = peek(tokens)
            if isinstance(next_token, TokenRParen):
                tokens.popleft()
                return group
            group.append(parse_parenths(tokens))
        raise RuntimeError("Unexpected end of tokens")
    else:
        raise RuntimeError(f"Unexpected token (when parsing parentheses): {token}")

def parse_expression(t: TokenNode) -> Expression:
    if isinstance(t, TokenInteger):
        return Integer(value=int(t.value))
    elif isinstance(t, TokenSymbol):
        return Symbol(t.value)
    elif isinstance(t, deque):
        operator = peek(t)
        if operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.LAMBDA.value:
            return parse_closure(t)
        elif operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.IF.value:
            return parse_if(t)
        else:
            return parse_invocation(t)
    else:
        raise RuntimeError(f"Unexpected token (when parsing expression): {t}")

def parse_closure(ts: deque[TokenNode]) -> Closure:
    if len(ts) != 3:
        raise RuntimeError("Malformed lambda expression")
    ts.popleft() # consume 'lambda'
    return Closure(
        parameters=list(map(parse_closure_parameter, ts.popleft())),
        body=parse_expression(ts.popleft()),
        environment=None
    )

def parse_closure_parameter(t: TokenNode) -> str:
    if isinstance(t, TokenSymbol):
        return t.value
    else:
        raise RuntimeError(f"Unexpected token in parameter list: {t}")

def parse_if(ts: deque[TokenNode]) -> If:
    if len(ts) != 4:
        raise RuntimeError("Malformed if expression")
    ts.popleft() # consume 'if'
    return If(
        condition=parse_expression(ts.popleft()), 
        consequent=parse_expression(ts.popleft()), 
        alternative=parse_expression(ts.popleft())
    )

def parse_invocation(ts: deque[TokenNode]) -> Invocation:
    if len(ts) == 0:
        raise RuntimeError("Malformed call expression")
    return Invocation(
        operator=parse_expression(ts.popleft()),
        arguments=list(map(parse_expression, ts))
    )

### EVALUATE ###

def evaluate(expression: Expression) -> Value:
    environment = base_environment()
    return evaluate_expression(expression, environment)

def base_environment() -> Frame:
    environment = Frame()
    for name, value in BUILTINS.items():
        environment.set(name, value)
    return environment

def evaluate_expression(expression: Expression, environment: Optional[Frame] = None) -> Value:
    if isinstance(expression, Integer):
        return int(expression.value)
    elif isinstance(expression, Symbol):
        if expression.value == Keyword.TRUE.value:
            return True
        elif expression.value == Keyword.FALSE.value:
            return False
        elif expression.value == Keyword.NONE.value:
            return None
        if environment is None:
            raise RuntimeError(f"Undefined identifier: {expression.value}")
        value = environment.get(expression.value)
        if value is None:
            raise RuntimeError(f"Undefined identifier: {expression.value}")
        return value
    elif isinstance(expression, Closure):
        return Closure(parameters=expression.parameters, body=expression.body, environment=environment)
    elif isinstance(expression, If):
        condition = evaluate_expression(expression.condition, environment)
        if condition:
            return evaluate_expression(expression.consequent, environment)
        else:
            return evaluate_expression(expression.alternative, environment)
    elif isinstance(expression, Invocation):
        return evaluate_invocation(expression, environment)
    else:
        logging.error("Cannot evaluate that expression quite yet")
        return None

def evaluate_invocation(expression: Invocation, environment: Frame) -> Value:
    operator = evaluate_expression(expression.operator, environment)
    if isinstance(operator, Callable):
        arguments = [evaluate_expression(arg, environment) for arg in expression.arguments]
        return operator(*arguments)
    elif isinstance(operator, Closure):
        child_environment = Frame(parent=operator.environment)
        if len(operator.parameters) != len(expression.arguments):
            raise RuntimeError("Argument count mismatch")
        for param, arg in zip(operator.parameters, expression.arguments):
            child_environment.set(param, evaluate_expression(arg, environment))
        return evaluate_expression(operator.body, child_environment)
    else:
        raise RuntimeError("Attempting to call a non-lambda expression")

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
                expression = parse(tokens)
                logging.debug(f"Parse result: {expression}")
            if expression:
                value = evaluate(expression)
                logging.debug(f"Evaluation result: {value}")
    except FileNotFoundError:
        logging.error(f"File not found: {source_file_path}")
        sys.exit(1)

if __name__ == "__main__":
    main()
