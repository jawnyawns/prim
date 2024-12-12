import logging
import string
import sys

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

def peek(l: list[T]) -> Optional[T]:
    return l[0] if l else None

### TOKENIZE ###

SPACE_CHARS = set(string.whitespace)
LPAREN_CHAR = "("
RPAREN_CHAR = ")"

INTEGER_CHAR_START = set(string.digits + "-")
SYMBOL_CHAR_START = set(string.ascii_lowercase)

INTEGER_CHAR_REST = set(string.digits)
SYMBOL_CHAR_REST = set(string.ascii_lowercase + string.digits + "_")

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
    A symbol is a string of characters that is a valid standalone datastructure whose value is itself (like a number or boolean).
    However, a symbol can be used to represent many things.
    - The name of an identifier
    - The name of an operator
    - A reserved keyword such as 'lambda'
    - A literal such as 'true' or 'none'
    - A built-in operator such as 'add'

    We merge all these possibilities into the singular concept of symbol because it is useful.
    - It simplifies lexing, for instance, it removes the need to distinguish between keywords, identifiers, etc.
    - It makes it easier for parser to convert tokens into a Pair(Pair(Pair(...))) representation.
    - It allows us to avoid threading verbose data structures through our program, instead, delaying the creation of
      those more descriptive types (Identifier/Lambda/etc) until they are actualy needed (e.g. during parse/eval).
    """
    value: str

def tokenize(source_code: str) -> list[Token]:
    tokens = []
    index = 0
    while index < len(source_code):
        character = source_code[index]
        if character in SPACE_CHARS:
            index += 1
        elif character == LPAREN_CHAR:
            tokens.append(TokenLParen())
            index += 1
        elif character == RPAREN_CHAR:
            tokens.append(TokenRParen())
            index += 1
        elif character in INTEGER_CHAR_START:
            text, index = consume_while(index, source_code, INTEGER_CHAR_REST)
            tokens.append(TokenInteger(value=int(text)))
        elif character in SYMBOL_CHAR_START:
            text, index = consume_while(index, source_code, SYMBOL_CHAR_REST)
            tokens.append(TokenSymbol(value=text))
        else:
            raise ValueError(f"Unexpected character '{character}' at position {index}")
    return tokens

def consume_while(start: int, source_code: str, valid_chars: set[str]) -> tuple[str, int]:
    end = start
    while end < len(source_code) and source_code[end] in valid_chars:
        end += 1
    text = source_code[start:end]
    return text, end

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
    # TODO: Rename to Frame
    # TODO: Rename values to bindings
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
    # TODO: Rename to Closure
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
    if isinstance(token, TokenSymbol):
        if token.value == Keyword.TRUE.value:
            return Boolean(value=True)
        elif token.value == Keyword.FALSE.value:
            return Boolean(value=False)
        else:
            return Identifier(name=token.value)
    elif isinstance(token, TokenInteger):
        return Number(value=int(token.value))
    elif isinstance(token, TokenLParen):
        operator = peek(tokens)
        if operator and isinstance(operator, TokenSymbol) and operator.value == Keyword.LAMBDA.value:
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
    if not isinstance(lambda_token, TokenSymbol) or lambda_token.value != Keyword.LAMBDA.value:
        raise RuntimeError(f"Expected 'lambda', got {lambda_token.value}")

    # parse parameters
    if not tokens or not isinstance(peek(tokens), TokenLParen):
        raise RuntimeError("Expected '(' to start parameter list")
    tokens.pop(0) # remove '('
    parameters = []
    while tokens and isinstance(peek(tokens), TokenSymbol):
        parameters.append(Identifier(name=tokens.pop(0).value))
    if not tokens or not isinstance(peek(tokens), TokenRParen):
        raise RuntimeError("Expected ')' to end parameter list")
    tokens.pop(0) # remove ')'

    # parse body
    body = parse(tokens)
    if not tokens or not isinstance(peek(tokens), TokenRParen):
        raise RuntimeError("Expected ')' to close lambda expression")
    tokens.pop(0) # remove ')'

    return Lambda(parameters=parameters, body=body, environment=None)

def parse_invocation(tokens: list[Token]) -> Expression:
    callable_expr = parse(tokens)
    arguments = []
    while tokens and not isinstance(peek(tokens), TokenRParen):
        arguments.append(parse(tokens))
    if not tokens or not isinstance(peek(tokens), TokenRParen):
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
