import logging
import sys

from dataclasses import dataclass
from enum import Enum
from typing import (
    Callable,
    Optional,
    TextIO,
    Union,
)

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

@dataclass
class Lambda(Expression):
    parameters: list[Identifier]
    body: Expression
    environment: dict[Identifier, Value]

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

def parse(tokens: list[Token]) -> Optional[Expression]:
    if not tokens:
        return None
    token = tokens[0]
    if token.type is TokenType.SYMBOL:
        if token.value == "false":
            return Boolean(value=False)
        elif token.value == "true":
            return Boolean(value=True)
        else:
            logging.error("Cannot parse non-bool symbol")
    elif token.type is TokenType.NUMBER:
        return Number(value=token.value)
    elif token.type is TokenType.LPAREN:
        # TODO: Implement lambda expression parsing
        pass
    else:
        logging.error("Cannot parse non-symbol token")

### EVALUATE ###

def evaluate(expression: Expression) -> Value:
    if isinstance(expression, Boolean):
        return bool(expression.value)
    elif isinstance(expression, Number):
        return int(expression.value)
    else:
        logging.error("Cannot evaluate that expression quite yet")

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
