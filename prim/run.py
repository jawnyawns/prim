import logging
import sys

from prim.lex import tokenize
from prim.ast import parse
from prim.eval import eval

def main() -> None:
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
                expr = parse(tokens)
                logging.debug(f"Parse result: {expr}")
            if expr:
                value = eval(expr)
                logging.debug(f"Evaluation result: {value}")
    except FileNotFoundError:
        logging.error(f"File not found: {source_file_path}")
        sys.exit(1)
