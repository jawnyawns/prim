import logging
import sys

from dataclasses import dataclass
from enum import Enum
from typing import (
    Optional,
    TextIO,
)

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
