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

KEYWORD_CHARS = set(string.ascii_lowercase)

INTEGER_CHAR_START = set(string.digits + "-")
IDENTIFIER_CHAR_START = set(string.ascii_lowercase)

INTEGER_CHAR_REST = set(string.digits)
IDENTIFIER_CHAR_REST = set(string.ascii_lowercase + string.digits + "_")

class Keywords(Enum):
    # literals
    TRUE = "true"
    FALSE = "false"
    NONE = "none"
    # reserved keywords
    LAMBDA = "lambda"

@dataclass
class Token:
    pass

# delimiters

@dataclass
class TokenLParen(Token):
    pass

@dataclass
class TokenRParen(Token):
    pass

# keywords

@dataclass
class TokenLambda(Token):
    pass

# literals

@dataclass
class TokenBoolean(Token):
    value: bool

@dataclass
class TokenInteger(Token):
    value: int

@dataclass
class TokenNone(Token):
    pass

# identifiers

@dataclass
class TokenIdentifier(Token):
    """
    Could be either an identifier bound to some runtime value, or,
    could refer to some built-in operator.
    """
    name: str


"""
TODO: Honestly, merging identifier, operator, lambda, boolean, none into SYMBOL makes sense...
      From a lexing POV these are all identical except that they have different textual values.
      The parsing can easily read the textual values of these symbols against some keyword list
      and derive the appropriate ASTNode.
"""

def tokenize(source_code: str) -> list[Token]:
    tokens = []
    index = 0
    while index < len(source_code):
        character = source_code[index]
        if character in SPACE_CHARS:
            index += 1
            break
        if character == LPAREN_CHAR:
            tokens.append(TokenLParen())
            index += 1
            break
        if character == RPAREN_CHAR:
            tokens.append(TokenRParen())
            index += 1
            break
        if character in KEYWORD_CHARS:
            text, index = scan_ahead(index, source_code, KEYWORD_CHARS)
            if text == Keywords.TRUE:
                tokens.append(TokenBoolean(value=True))
                break
            elif text == Keywords.FALSE:
                tokens.append(TokenBoolean(value=False))
                break
            elif text == Keywords.NONE:
                tokens.append(TokenNone())
                break
            elif text == Keywords.LAMBDA:
                tokens.append(TokenLambda())
                break
        if character in INTEGER_CHAR_START:
            text, index = scan_ahead(index, source_code, INTEGER_CHAR_REST)
            tokens.append(TokenInteger(value=int(text)))
            break
        if character in IDENTIFIER_CHAR_START:
            text, index = scan_ahead(index, source_code, IDENTIFIER_CHAR_REST)
            tokens.append(TokenIdentifier(name=text))
            break
        raise ValueError(f"Unexpected character '{character}' at position {index}")
    return tokens

def scan_ahead(start: int, source_code: str, valid_chars: set[str]) -> tuple[Token, int]:
    end = start
    while end < len(source_code) and source_code[end] in valid_chars:
        end += 1
    value = source_code[start:end]
    return value, end

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
