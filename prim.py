import logging
import string
import sys

from dataclasses import dataclass
from enum import Enum
from typing import (
    Callable,
    Optional,
    Union,
)

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

class Expression:
    pass

@dataclass
class Integer(Expression):
    value: int

@dataclass
class Symbol(Expression):
    value: str

@dataclass
class Lambda(Expression):
    parameters: list[str]
    body: Expression

@dataclass
class Call(Expression):
    operator: Expression
    arguments: list[Expression]

@dataclass
class If(Expression):
    condition: Expression
    consequent: Expression
    alternative: Expression

TokenNode = Union[TokenNonParen, list["TokenNode"]]

def parse(tokens: list[Token]) -> Expression:
    token_node, _ = parse_parens(tokens)
    expression, _ = parse_expression(token_node)
    return expression

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

def parse_expression(t: TokenNode) -> tuple[Expression, TokenNode]:
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
        body=parse_expression(body)[0],
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
        condition=parse_expression(condition)[0], 
        consequent=parse_expression(consequent)[0], 
        alternative=parse_expression(alternative)[0]
    ), rest

def parse_call(t: TokenNode) -> tuple[Call, TokenNode]:
    if len(t) == 0:
        raise RuntimeError("Malformed call expression")
    operator, *rest = t
    return Call(
        operator=parse_expression(operator)[0],
        arguments=list(map(lambda t: parse_expression(t)[0], rest))
    ), rest

### EVALUATE ###

Value = Union[int, bool, Callable, "Closure", None]

@dataclass
class Closure:
    parameters: list[str]
    body: Expression
    environment: "Frame"

@dataclass
class Frame:
    bindings: dict[str, Value]
    parent: Optional["Frame"]

    def get(self, name: str) -> Optional[Value]:
        if name in self.bindings:
            return self.bindings[name]
        elif self.parent:
            return self.parent.get(name)
        else:
            return None

BUILTINS: dict[str, Callable] = {
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
    "list": lambda *args: list(args),
}

def evaluate(expression: Expression) -> Value:
    environment = base_environment()
    return evaluate_expression(expression, environment)

def base_environment() -> Frame:
    return Frame(bindings=BUILTINS, parent=None)

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
    elif isinstance(expression, Lambda):
        return Closure(parameters=expression.parameters, body=expression.body, environment=environment)
    elif isinstance(expression, If):
        condition = evaluate_expression(expression.condition, environment)
        if condition:
            return evaluate_expression(expression.consequent, environment)
        else:
            return evaluate_expression(expression.alternative, environment)
    elif isinstance(expression, Call):
        return evaluate_call(expression, environment)
    else:
        raise RuntimeError(f"Unsupported expression: {expression}")

def evaluate_call(expression: Call, environment: Frame) -> Value:
    operator = evaluate_expression(expression.operator, environment)
    if isinstance(operator, Callable):
        arguments = [evaluate_expression(arg, environment) for arg in expression.arguments]
        return operator(*arguments)
    elif isinstance(operator, Closure):
        if len(operator.parameters) != len(expression.arguments):
            raise RuntimeError("Argument count mismatch")
        bindings = {
            param: evaluate_expression(arg, environment) for param, arg in zip(operator.parameters, expression.arguments)
        }
        child_environment = Frame(
            bindings=bindings,
            parent=operator.environment
        )
        return evaluate_expression(operator.body, child_environment)
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
