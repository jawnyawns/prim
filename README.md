# Prim

Let's make a language!

## Usage

```
python3 prim.py examples/lambda_builtins.prim
```

## Test

```
python3 -m unittest discover -s test -p "test_*.py"
```

## Features

- Booleans
- Numbers
- Lexically scoped closure-supporting lambda functions (and yes, functions should be values)
- Basic math
- Boolean expressions (e.g. equals, less than, etc)

## TODO

- Refactor parser to create Pair(Pair(Pair(...))) data structure since syntax of Prim is so uniform?
- Pairs (try implementing direclty in Prim via lambdas)
- Lists (try implementing direclty in Prim via lambdas)
- Variable binding and recall via let expressions only (which are implemented via lambdas, no define!)
- Dynamic type checking and corresponding runtime errors
- Core library implemented exclusively in Prim (e.g. if/else, and/or, not, let)
- NO exceptions. Only return types.

## Things to try out in the future

- Lazy-eval by default
- Support a built-in dict data structure
- Optional parenths: indentation means "within parenth" and new line means "new item within parenth"

## License

© John Chin-Jew
