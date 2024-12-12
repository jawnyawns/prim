import logging
import string
import sys

from collections import deque
from dataclasses import dataclass
from enum import Enum
from typing import (
    Callable,
    Optional,
    TypeVar,
    Union,
)

### UTILS ###

T = TypeVar("T")

def peek(l: deque[T]) -> Optional[T]:
    return l[0] if l else None

### TOKENIZE ###

SPACE_CHARS = set(string.whitespace)
LPAREN_CHARS = set("(")
RPAREN_CHARS = set(")")
INTEGER_CHAR_START = set(string.digits + "-")
SYMBOL_CHAR_START = set(string.ascii_lowercase)
INTEGER_CHAR_REST = set(string.digits)
SYMBOL_CHAR_REST = set(string.ascii_lowercase + string.digits + "_")
INTEGER_CHAR_END = SPACE_CHARS | LPAREN_CHARS | RPAREN_CHARS
SYMBOL_CHAR_END = SPACE_CHARS | LPAREN_CHARS | RPAREN_CHARS

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
class TokenInteger(Token):
    value: int

@dataclass
class TokenSymbol(Token):
    """
    A symbol is a sequence of characters that can represent many things:
    - The name of an identifier
    - The name of a built-in operator
    - The textual representation of a literal
    - A reserved keyword

    Merging all these into a single token is useful. It simplifies lexing and parsing,
    and, avoids threading overly verbose data structures throughout our implementation.
    """
    value: str

def tokenize(source_code: str) -> deque[Token]:
    tokens = deque()
    index = 0
    while index < len(source_code):
        character = source_code[index]
        if character in SPACE_CHARS:
            index += 1
        elif character in LPAREN_CHARS:
            tokens.append(TokenLParen())
            index += 1
        elif character in RPAREN_CHARS:
            tokens.append(TokenRParen())
            index += 1
        elif character in INTEGER_CHAR_START:
            text, index = consume_until_delimiter(index, source_code, INTEGER_CHAR_END)
            if is_valid_integer(text):
                tokens.append(TokenInteger(value=int(text)))
            else:
                raise RuntimeError(f"Invalid integer '{text}' preceding position {index}")
        elif character in SYMBOL_CHAR_START:
            text, index = consume_until_delimiter(index, source_code, SYMBOL_CHAR_END)
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
    return text and text[0] in INTEGER_CHAR_START and all(c in INTEGER_CHAR_REST for c in text[1:])

def is_valid_symbol(text: str) -> bool:
    return text and text[0] in SYMBOL_CHAR_START and all(c in SYMBOL_CHAR_REST for c in text[1:])

### PARSE ###

class Keyword(Enum):
    TRUE = "true"
    FALSE = "false"
    NONE = "none"
    LAMBDA = "lambda"

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
        self.bindings: dict[Identifier, Value] = {}
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
class Boolean(Expression):
    value: bool

@dataclass
class Integer(Expression):
    value: int

@dataclass
class Identifier(Expression):
    name: str

@dataclass
class Closure(Expression):
    parameters: list[Identifier]
    body: Expression
    frame: Frame

@dataclass
class Invocation(Expression):
    operator: Expression
    arguments: list[Expression]

def parse(tokens: deque[Token]) -> Optional[Expression]:
    if not tokens:
        return None
    token = tokens.popleft()
    if isinstance(token, TokenSymbol):
        if token.value == Keyword.TRUE.value:
            return Boolean(value=True)
        elif token.value == Keyword.FALSE.value:
            return Boolean(value=False)
        else:
            return Identifier(name=token.value)
    elif isinstance(token, TokenInteger):
        return Integer(value=int(token.value))
    elif isinstance(token, TokenLParen):
        operator = peek(tokens)
        if operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.LAMBDA.value:
            return parse_closure(tokens)
        else:
            return parse_invocation(tokens)
    else:
        logging.error("Cannot parse non-symbol token")
        return None

def parse_closure(tokens: deque[Token]):
    if not tokens:
        raise RuntimeError("Unexpected end of tokens after '('")
    lambda_token = tokens.popleft()
    if not isinstance(lambda_token, TokenSymbol) or lambda_token.value != Keyword.LAMBDA.value:
        raise RuntimeError(f"Expected 'lambda', got {lambda_token.value}")

    if not tokens or not isinstance(peek(tokens), TokenLParen):
        raise RuntimeError("Expected '(' to start parameter list")
    parameters = []
    tokens.popleft()
    while tokens and isinstance(peek(tokens), TokenSymbol):
        parameters.append(Identifier(name=tokens.popleft().value))
    if not tokens or not isinstance(peek(tokens), TokenRParen):
        raise RuntimeError("Expected ')' to end parameter list")
    tokens.popleft()

    body = parse(tokens)
    if not tokens or not isinstance(peek(tokens), TokenRParen):
        raise RuntimeError("Expected ')' to close lambda expression")
    tokens.popleft()

    return Closure(parameters=parameters, body=body, frame=None)

def parse_invocation(tokens: deque[Token]) -> Expression:
    callable_expr = parse(tokens)
    arguments = []
    while tokens and not isinstance(peek(tokens), TokenRParen):
        arguments.append(parse(tokens))
    if not tokens or not isinstance(peek(tokens), TokenRParen):
        raise RuntimeError("Expected ')' to close invocation")
    tokens.popleft()
    return Invocation(operator=callable_expr, arguments=arguments)

### EVALUATE ###

def evaluate(expression: Expression, environment: Optional[Frame] = None) -> Value:
    if isinstance(expression, Boolean):
        return bool(expression.value)
    elif isinstance(expression, Integer):
        return int(expression.value)
    elif isinstance(expression, Identifier):
        if expression.name in BUILTINS:
            return expression
        if environment is None:
            raise RuntimeError(f"Undefined identifier: {expression.name}")
        value = environment.get(expression.name)
        if value is None:
            raise RuntimeError(f"Undefined identifier: {expression.name}")
        return value
    elif isinstance(expression, Closure):
        return Closure(parameters=expression.parameters, body=expression.body, frame=environment)
    elif isinstance(expression, Invocation):
        return evaluate_invocation(expression, environment)
    else:
        logging.error("Cannot evaluate that expression quite yet")
        return None

def evaluate_invocation(expression: Invocation, environment: Frame) -> Value:
    operator = evaluate(expression.operator, environment)
    if isinstance(operator, Identifier) and operator.name in BUILTINS:
        arguments = [evaluate(arg, environment) for arg in expression.arguments]
        return BUILTINS[operator.name](*arguments)
    elif isinstance(operator, Closure):
        new_env = Frame(parent=operator.frame)
        if len(operator.parameters) != len(expression.arguments):
            raise RuntimeError("Argument count mismatch")
        for param, arg in zip(operator.parameters, expression.arguments):
            new_env.set(param.name, evaluate(arg, environment))
        return evaluate(operator.body, new_env)
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
