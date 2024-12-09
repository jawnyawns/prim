import sys

from enum import Enum
from typing import (
    TextIO,
)

# TODO: Add debug level logging
# TODO: Add type hints
# TODO: Refactor tokenization

### CONSTANTS ###

class CharSet(Enum):
    SYMBOL = set("abcdefghijklmnopqrstuvwxyz_")
    NUMBER = set("0123456789")
    LPAREN = set("(")
    RPAREN = set(")")
    SPACE = set(" \n\t")

class Token(Enum):
    LPAREN = "LPAREN"
    RPAREN = "RPAREN"
    SYMBOL = "SYMBOL"
    NUMBER = "NUMBER"

### TOKENIZATION ###

def tokenize(source_code: str):
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

def next_token(i, source_code):
    results = [
        tokenize_space(i, source_code),
        tokenize_symbol(i, source_code),
        tokenize_number(i, source_code),
        tokenize_paren(i, source_code),
    ]

    def is_success(result):
        next_i, _ = result
        return next_i > i

    return next(filter(is_success, results), None)

def tokenize_space(i, source_code):
    char = source_code[i]
    if char in CharSet.SPACE.value:
        return i + 1, None
    else:
        return i, None

def tokenize_symbol(i, source_code):
    if source_code[i] in CharSet.SYMBOL.value:
        symbol = ""
        while i < len(source_code) and source_code[i] in CharSet.SYMBOL.value:
            symbol += source_code[i]
            i += 1
        return i, (Token.SYMBOL, symbol)
    else:
        return i, None

def tokenize_number(i, source_code):
    if source_code[i] in CharSet.NUMBER.value:
        number = ""
        while i < len(source_code) and source_code[i] in CharSet.NUMBER.value:
            number += source_code[i]
            i += 1
        return i, (Token.NUMBER, int(number))
    else:
        return i, None

def tokenize_paren(i, source_code):
    char = source_code[i]
    if char in CharSet.LPAREN.value:
        return i + 1, (Token.LPAREN, char)
    elif char in CharSet.RPAREN.value:
        return i + 1, (Token.RPAREN, char)
    else:
        return i, None

### PARSING ###

def parse(tokens):
    print("Parsing tokens into AST is not yet implemented!")

### EVALUATION ###

def evaluate(ast):
    print("Evaluating AST is not yet implemented!")

### MAIN ###

def main():
    source_file_path = get_source_file_path(sys.argv)
    with open(source_file_path, "r", encoding="utf-8") as source_file:
        execute_source_file(source_file)

def get_source_file_path(argv: list[str]):
    if len(argv) == 2:
        return argv[1]
    else:
        print("Usage: python prim.py <FILE_PATH>")

def execute_source_file(source_file: TextIO):
    source_code = source_file.read()
    source_tokens = tokenize(source_code)

    print(f"Debug: Tokenization success: {source_tokens}")

    source_ast = parse(source_tokens)
    evaluate(source_ast)

if __name__ == "__main__":
    main()
