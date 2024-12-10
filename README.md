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

- Pairs (try implementing direclty in Prim via lambdas)
- Lists (try implementing direclty in Prim via lambdas)
- Variable binding and recall via let expressions only (which are implemented via lambdas, no define!)
- Dynamic type checking and corresponding runtime errors
- Core library implemented exclusively in Prim (e.g. if/else, and/or, not, let)

## Things to try out in the future

- Lazy-eval by default
- Support a built-in dict data structure

## License

© John Chin-Jew
