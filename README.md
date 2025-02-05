# Prim

Prim is a programming language I am building. It is experimental, expect instability and breaking changes!

*For more details, see the [working doc](https://docs.google.com/document/d/1iJsffAytYFPaUuvtak5jFNhN2Ew4YV_O9aWLTrtU4vw/edit?usp=sharing).*

## Usage

Requires **Python 3.12** or higher.

Here's an example of how to execute a Prim source code file via the Prim interpreter.

```
python3 -m prim examples/factorial.prim
```

## Development

Run unit tests.

```
python3 -m unittest discover -s test/*
```

Run type checking.

```
mypy -p prim --strict
```

## Copyright

© John Chin-Jew. All rights reserved.
