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

class Expression:
    pass

@dataclass
class Boolean(Expression):
    value: bool

def parse(tokens: list[Token]) -> Optional[Expression]:
    expr = None
    for token in tokens:
        if token.type is TokenType.SYMBOL:
            if token.value == "false":
                expr = Boolean(value=False)
            elif token.value == "true":
                expr = Boolean(value=True)
            else:
                logging.error("Cannot parse non-bool symbol")
        else:
            logging.error("Cannot parse non-symbol token")
    return expr

### EVALUATE ###

def evaluate(expression: Expression) -> Union[int, float, str, bool, list, Callable, None]:
    if isinstance(expression, Boolean):
        return evaluate_boolean(expression)
    else:
        logging.error("Cannot evaluate expression that is not a boolean")

def evaluate_boolean(expression: Boolean) -> bool:
    return bool(expression.value)

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
