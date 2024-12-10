# Prim

Let's make a language!

## Usage

```
python3 prim.py examples/factorial.prim
```

## Testing

```
python3 -m unittest discover -s test -p "test_*.py"
```

## Features

- Booleans
- Numbers
- Lists
- Basic math
- Boolean expressions (e.g. equals, less than, etc)
- Lexically scoped closure-supporting lambda functions (and yes, functions should be values)
- Variable binding and recall via let expressions only (which are implemented via lambdas, no define!)
- Lazy-eval by default
- Dynamic type checking and corresponding runtime errors
- Core library implemented exclusively in Prim (e.g. if/else, and/or, not, let)

## License

© John Chin-Jew
