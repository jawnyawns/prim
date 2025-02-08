import logging

from prim.lex import tokenize
from prim.ast import parse
from prim.eval import (eval, Value)

def exec(source_code: str) -> Value:
    if source_code:
        tokens = tokenize(source_code)
        logging.debug(f"Tokens: {tokens}")
    if tokens:
        ast = parse(tokens)
        logging.debug(f"AST: {ast}")
    if ast:
        value = eval(ast)
        logging.debug(f"Value: {value}")
    return value
