import sys

SYMBOL_CHARS = "abcdefghijklmnopqrstuvwxyz_"
NUMBER_CHARS = "0123456789"
L_PAREN_CHAR = "("
R_PAREN_CHAR = ")"
SPACE_CHARS = " \n"

TOKEN_L_PAREN = "L_PAREN"
TOKEN_R_PAREN = "R_PAREN"
TOKEN_SYMBOL = "SYMBOL"
TOKEN_NUMBER = "NUMBER"

def main():
    if len(sys.argv) != 2:
        print("Usage: python prim.py <SOURCE_FILE_PATH>")
    else:
        source_file_path = sys.argv[1]

        with open(source_file_path, "r", encoding="utf-8") as source_file:
            source_code = source_file.read()
            source_tokens = tokenize(source_code)

            print(f"Tokenization success: {source_tokens}")

            source_ast = parse(source_tokens)
            evaluate(source_ast)

### TOKENIZATION ###

def tokenize(source_code):
    tokens = []
    i = 0
    while i < len(source_code):
        result = next_token(i, source_code)
        if result:
            i, token = result
            if token:
                tokens.append(token)
        else:
            raise Exception(f"Tokenization error: Invalid character {source_code[i]} at position {i}")
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
    if char in SPACE_CHARS:
        return i + 1, None
    else:
        return i, None

def tokenize_symbol(i, source_code):
    if source_code[i] in SYMBOL_CHARS:
        symbol = ""
        while i < len(source_code) and source_code[i] in SYMBOL_CHARS:
            symbol += source_code[i]
            i += 1
        return i, (TOKEN_SYMBOL, symbol)
    else:
        return i, None

def tokenize_number(i, source_code):
    if source_code[i] in NUMBER_CHARS:
        number = ""
        while i < len(source_code) and source_code[i] in NUMBER_CHARS:
            number += source_code[i]
            i += 1
        return i, (TOKEN_NUMBER, int(number))
    else:
        return i, None

def tokenize_paren(i, source_code):
    char = source_code[i]
    if char == L_PAREN_CHAR:
        return i + 1, (TOKEN_L_PAREN, char)
    elif char == R_PAREN_CHAR:
        return i + 1, (TOKEN_R_PAREN, char)
    else:
        return i, None

### PARSING ###

def parse(tokens):
    print("Parsing tokens into AST is not yet implemented!")

### EVALUATION ###

def evaluate(ast):
    print("Evaluating AST is not yet implemented!")

if __name__ == "__main__":
    main()
