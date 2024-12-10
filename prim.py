import logging
import sys

from dataclasses import dataclass
from enum import Enum
from typing import (
    Optional,
    TextIO,
)

# TODO: Refactor tokenization

### CONSTANTS ###

LOG_FORMAT = "[%(levelname)s] %(message)s"

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

### TOKENIZE ###

def tokenize(source_code: str) -> list[Token]:
    tokens = []
    i = 0
    while i < len(source_code):
        result = next_token(i, source_code)
        if result:
            i, token = result
            if token:
                tokens.append(token)
        else:
            raise Exception(f"Tokenization error: Last valid token was {tokens[-1]} at position {i}")
    return tokens

def next_token(i: int, source_code: str) -> Optional[tuple[int, Optional[Token]]]:
    results = [
        next_token_space(i, source_code),
        next_token_symbol(i, source_code),
        next_token_number(i, source_code),
        next_token_paren(i, source_code),
    ]

    def is_success(result):
        next_i, _ = result
        return next_i > i

    return next(filter(is_success, results), None)

def next_token_space(i: int, source_code: str) -> tuple[int, Optional[Token]]:
    char = source_code[i]
    if char in CharSet.SPACE.value:
        return i + 1, None
    else:
        return i, None

def next_token_symbol(i: int, source_code: str) -> tuple[int, Optional[Token]]:
    if source_code[i] in CharSet.SYMBOL.value:
        symbol = ""
        while i < len(source_code) and source_code[i] in CharSet.SYMBOL.value:
            symbol += source_code[i]
            i += 1
        return i, Token(TokenType.SYMBOL, symbol)
    else:
        return i, None

def next_token_number(i: int, source_code: str) -> tuple[int, Optional[Token]]:
    if source_code[i] in CharSet.NUMBER.value:
        number = ""
        while i < len(source_code) and source_code[i] in CharSet.NUMBER.value:
            number += source_code[i]
            i += 1
        return i, Token(TokenType.NUMBER, number)
    else:
        return i, None

def next_token_paren(i: int, source_code: str) -> tuple[int, Optional[Token]]:
    char = source_code[i]
    if char in CharSet.LPAREN.value:
        return i + 1, Token(TokenType.LPAREN, char)
    elif char in CharSet.RPAREN.value:
        return i + 1, Token(TokenType.RPAREN, char)
    else:
        return i, None

### PARSE ###

class Node:
    pass

def parse(tokens: list[Token]):
    logging.error("Parsing tokens into AST is not yet implemented!")

### EVALUATE ###

def evaluate(ast: Node):
    logging.error("Evaluating AST is not yet implemented!")

### MAIN ###

def main():
    logging.basicConfig(level=logging.DEBUG, format=LOG_FORMAT)
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
    source_tokens = tokenize(source_code)
    logging.debug(f"Tokenization success: {source_tokens}")
    source_ast = parse(source_tokens)
    evaluate(source_ast)

if __name__ == "__main__":
    main()
