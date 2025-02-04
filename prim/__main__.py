import argparse
import logging

from prim.lex import tokenize
from prim.ast import parse
from prim.eval import eval

arg_parser = argparse.ArgumentParser()
arg_parser.add_argument("--verbose", action="store_true", help="enable verbose logs")
arg_parser.add_argument("file", type=str, help="path to prim source file")
args = arg_parser.parse_args()

if args.verbose:
    logging.basicConfig(level=logging.DEBUG, format="[%(levelname)s] %(message)s")
else:
    logging.disable()

with open(args.file, "r", encoding="utf-8") as source_file:
    source_code = source_file.read()

if source_code:
    tokens = tokenize(source_code)
    logging.debug(f"Tokens: {tokens}")
if tokens:
    ast = parse(tokens)
    logging.debug(f"AST: {ast}")
if ast:
    value = eval(ast)
    logging.debug(f"Value: {value}")

if value:
    print(value)
