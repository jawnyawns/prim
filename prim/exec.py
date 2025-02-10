import logging

from prim.lex import tokenize
from prim.ast import parse
from prim.eval import (eval, Value)

def exec(source_code: str) -> list[Value]:
    if source_code:
        tokens = tokenize(source_code)
        logging.debug(f"Tokens: {tokens}")
    if tokens:
        exprs = parse(tokens)
        logging.debug(f"Expressions: {exprs}")
    if exprs:
        values = eval(exprs)
        logging.debug(f"Values: {values}")
    return values
