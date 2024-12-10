import logging
import sys

from dataclasses import dataclass
from enum import Enum
from typing import (
    Callable,
    Optional,
    TextIO,
    TypeVar,
    Union,
)

### UTILS ###

T = TypeVar("T")

def peek(l: list[T]) -> Optional[T]:
    return l[0] if l else None

### TOKENIZE ###

class CharSet(Enum):
    SYMBOL = set("abcdefghijklmnopqrstuvwxyz_")
    NUMBER = set("0123456789")
    LPAREN = set("(")
    RPAREN = set(")")
    SPACE = set(" \n\t")

class TokenType(Enum):
    LPAREN = "LPAREN"
    RPAREN = "RPAREN"
    SYMBOL = "SYMBOL"
    NUMBER = "NUMBER"

@dataclass(frozen=True)
class Token:
    type: TokenType
    value: str

def tokenize(source_code: str) -> list[Token]:
    tokens = []
    index = 0
    while index < len(source_code):
        character = source_code[index]
        if character in CharSet.SPACE.value:
            # skip whitespace
            index += 1
        elif character in CharSet.LPAREN.value:
            tokens.append(Token(TokenType.LPAREN, character))
            index += 1
        elif character in CharSet.RPAREN.value:
            tokens.append(Token(TokenType.RPAREN, character))
            index += 1
        elif character in CharSet.SYMBOL.value:
            token, index = consume_while(index, source_code, CharSet.SYMBOL.value, TokenType.SYMBOL)
            tokens.append(token)
        elif character in CharSet.NUMBER.value:
            token, index = consume_while(index, source_code, CharSet.NUMBER.value, TokenType.NUMBER)
            tokens.append(token)
        else:
            raise ValueError(f"Unexpected character '{character}' at position {index}")
    return tokens

def consume_while(start: int, source_code: str, valid_chars: set[str], token_type: TokenType) -> tuple[Token, int]:
    end = start
    while end < len(source_code) and source_code[end] in valid_chars:
        end += 1
    value = source_code[start:end]
    return Token(token_type, value), end

### PARSE ###

Value = Union[int, bool, Callable, None]

BUILTINS = {
    "add": lambda a, b: a + b,
    "subtract": lambda a, b: a - b,
    "multiply": lambda a, b: a * b,
    "equals": lambda a, b: a == b,
    "less_than": lambda a, b: a < b,
    "greater_than": lambda a, b: a > b,
    "less_than_or_equals": lambda a, b: a <= b,
    "greater_than_or_equals": lambda a, b: a >= b,
}

class Expression:
    pass

@dataclass
class Boolean(Expression):
    value: bool

@dataclass
class Number(Expression):
    value: int

@dataclass
class Identifier(Expression):
    name: str

class Environment:
    def __init__(self, parent: Optional["Environment"] = None):
        self.values = {}
        self.parent = parent

    def get(self, name: str) -> Optional[Value]:
        if name in self.values:
            return self.values[name]
        elif self.parent:
            return self.parent.get(name)
        else:
            return None

    def set(self, name: str, value: Value):
        self.values[name] = value

@dataclass
class Lambda(Expression):
    parameters: list[Identifier]
    body: Expression
    environment: Environment

@dataclass
class Invocation(Expression):
    operator: Expression
    arguments: list[Expression]

# TODO: Use deque for popleft() instead of pop(0)

def parse(tokens: list[Token]) -> Optional[Expression]:
    if not tokens:
        return None
    token = tokens.pop(0)
    if token.type is TokenType.SYMBOL:
        # TODO: Can probably make "true/false" stand-alone "keyword" tokens
        if token.value == "false":
            return Boolean(value=False)
        elif token.value == "true":
            return Boolean(value=True)
        else:
            # TODO: How to differentiate identifier from other symbols?
            return Identifier(name=token.value)
    elif token.type is TokenType.NUMBER:
        return Number(value=int(token.value))
    elif token.type is TokenType.LPAREN:
        operator = peek(tokens)
        if operator and operator.type is TokenType.SYMBOL and operator.value == "lambda":
            return parse_lambda(tokens)
        else:
            return parse_invocation(tokens)
    else:
        logging.error("Cannot parse non-symbol token")
        return None

def parse_lambda(tokens: list[Token]):
    if not tokens:
        raise RuntimeError("Unexpected end of tokens after '('")
    lambda_token = tokens.pop(0)
    if lambda_token.type is not TokenType.SYMBOL or lambda_token.value != "lambda":
        raise RuntimeError(f"Expected 'lambda', got {lambda_token.value}")

    # parse parameters
    if not tokens or peek(tokens).type is not TokenType.LPAREN:
        raise RuntimeError("Expected '(' to start parameter list")
    tokens.pop(0) # remove '('
    parameters = []
    while tokens and peek(tokens).type is TokenType.SYMBOL:
        parameters.append(Identifier(name=tokens.pop(0).value))
    if not tokens or peek(tokens).type is not TokenType.RPAREN:
        raise RuntimeError("Expected ')' to end parameter list")
    tokens.pop(0) # remove ')'

    # parse body
    body = parse(tokens)
    if not tokens or peek(tokens).type is not TokenType.RPAREN:
        raise RuntimeError("Expected ')' to close lambda expression")
    tokens.pop(0) # remove ')'

    return Lambda(parameters=parameters, body=body, environment=None)

def parse_invocation(tokens: list[Token]) -> Expression:
    callable_expr = parse(tokens)
    arguments = []
    while tokens and peek(tokens).type is not TokenType.RPAREN:
        arguments.append(parse(tokens))
    if not tokens or peek(tokens).type is not TokenType.RPAREN:
        raise RuntimeError("Expected ')' to close invocation")
    tokens.pop(0) # remove ')'
    return Invocation(operator=callable_expr, arguments=arguments)

### EVALUATE ###

def evaluate(expression: Expression, environment: Optional[Environment] = None) -> Value:
    if isinstance(expression, Boolean):
        return bool(expression.value)
    elif isinstance(expression, Number):
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
    elif isinstance(expression, Lambda):
        return Lambda(parameters=expression.parameters, body=expression.body, environment=environment)
    elif isinstance(expression, Invocation):
        return evaluate_invocation(expression, environment)
    else:
        logging.error("Cannot evaluate that expression quite yet")
        return None

def evaluate_invocation(expression: Invocation, environment: Environment) -> Value:
    # evaluate callable
    operator = evaluate(expression.operator, environment)
    if isinstance(operator, Identifier) and operator.name in BUILTINS:
        arguments = [evaluate(arg, environment) for arg in expression.arguments]
        return BUILTINS[operator.name](*arguments)
    elif isinstance(operator, Lambda):
        # create new environment for lambda execution
        new_env = Environment(parent=operator.environment)
        if len(operator.parameters) != len(expression.arguments):
            raise RuntimeError("Argument count mismatch")
        for param, arg in zip(operator.parameters, expression.arguments):
            new_env.set(param.name, evaluate(arg, environment))
        # evaluate the lambda body in the new environment
        return evaluate(operator.body, new_env)
    else:
        raise RuntimeError("Attempting to call a non-lambda expression")

### MAIN ###

def main():
    logging.basicConfig(level=logging.DEBUG, format="[%(levelname)s] %(message)s")
    # TODO: What if there is no source file path?
    source_file_path = get_source_file_path(sys.argv)
    # TODO: What if the file does not exist?
    with open(source_file_path, "r", encoding="utf-8") as source_file:
        execute_source_file(source_file)

def get_source_file_path(argv: list[str]) -> Optional[str]:
    if len(argv) == 2:
        return argv[1]
    else:
        logging.error("Usage: python prim.py <FILE_PATH>")
        return None

def execute_source_file(source_file: TextIO):
    source_code = source_file.read()
    tokens = tokenize(source_code)
    logging.debug(f"Tokenize result: {tokens}")
    expression = parse(tokens)
    logging.debug(f"Parse result: {expression}")
    if expression:
        value = evaluate(expression)
        logging.debug(f"Evaluation result: {value}")

if __name__ == "__main__":
    main()
