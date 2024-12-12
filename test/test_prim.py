from prim import (
    Boolean,
    evaluate,
    Identifier,
    Invocation,
    Lambda,
    Number,
    parse,
    TokenLParen,
    TokenRParen,
    TokenSymbol,
    TokenInteger,
    tokenize,
)
from unittest import (
    main,
    TestCase,
)

class TestTokenize(TestCase):
    def test_empty(self):
        self.assertEqual(tokenize(""), [])

    def test_symbol(self):
        self.assertEqual(tokenize("x"), [TokenSymbol("x")])

    def test_number(self):
        self.assertEqual(tokenize("123"), [TokenInteger(123)])

    def test_parentheses(self):
        self.assertEqual(
            tokenize("()"),
            [
                TokenLParen(),
                TokenRParen(),
            ]
        )

    def test_let(self):
        self.assertEqual(
            tokenize("(let x 5)"),
            [
                TokenLParen(),
                TokenSymbol("let"),
                TokenSymbol("x"),
                TokenInteger(5),
                TokenRParen(),
            ]
        )

    def test_complex_example(self):
        source_code = """(
  let (
    (factorial (lambda (n) (if (eq n 0) 1 (multiply n (factorial (subtract n 1))))))
    (n 5)
  )
  (factorial n)
)
"""
        actual_tokens = tokenize(source_code)
        expected_tokens = [
            TokenLParen(),
            TokenSymbol("let"),
            TokenLParen(),
            TokenLParen(),
            TokenSymbol("factorial"),
            TokenLParen(),
            TokenSymbol("lambda"),
            TokenLParen(),
            TokenSymbol("n"),
            TokenRParen(),
            TokenLParen(),
            TokenSymbol("if"),
            TokenLParen(),
            TokenSymbol("eq"),
            TokenSymbol("n"),
            TokenInteger(0),
            TokenRParen(),
            TokenInteger(1),
            TokenLParen(),
            TokenSymbol("multiply"),
            TokenSymbol("n"),
            TokenLParen(),
            TokenSymbol("factorial"),
            TokenLParen(),
            TokenSymbol("subtract"),
            TokenSymbol("n"),
            TokenInteger(1),
            TokenRParen(),
            TokenRParen(),
            TokenRParen(),
            TokenRParen(),
            TokenRParen(),
            TokenRParen(),
            TokenLParen(),
            TokenSymbol("n"),
            TokenInteger(5),
            TokenRParen(),
            TokenRParen(),
            TokenLParen(),
            TokenSymbol("factorial"),
            TokenSymbol("n"),
            TokenRParen(),
            TokenRParen(),
        ]
        self.assertEqual(actual_tokens, expected_tokens)

class TestParse(TestCase):
    def test_empty(self):
        self.assertEqual(parse([]), None)

    def test_boolean(self):
        self.assertEqual(parse([TokenSymbol("true")]), Boolean(True))
        self.assertEqual(parse([TokenSymbol("false")]), Boolean(False))
    
    def test_number(self):
        self.assertEqual(parse([TokenInteger(123)]), Number(123))
    
    def test_lambda(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenSymbol('lambda'),
                    TokenLParen(),
                    TokenSymbol('x'),
                    TokenRParen(),
                    TokenSymbol('x'),
                    TokenRParen(),
                ]
            ),
            Lambda(parameters=[Identifier('x')], body=Identifier('x'), environment=None)
        )
    
    def test_lambda_invocation(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenLParen(),
                    TokenSymbol('lambda'),
                    TokenLParen(),
                    TokenSymbol('x'),
                    TokenRParen(),
                    TokenSymbol('x'),
                    TokenRParen(),
                    TokenInteger(1),
                    TokenRParen(),
                ]
            ),
            Invocation(operator=Lambda(parameters=[Identifier('x')], body=Identifier('x'), environment=None), arguments=[Number(1)])
        )
    
    def test_builtins(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenSymbol('add'),
                    TokenInteger(1),
                    TokenInteger(2),
                    TokenRParen(),
                ]
            ),
            Invocation(operator=Identifier('add'), arguments=[Number(1), Number(2)])
        )
    
    def test_boolean_expressions(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenSymbol('eq'),
                    TokenInteger(1),
                    TokenInteger(1),
                    TokenRParen(),
                ]
            ),
            Invocation(operator=Identifier('eq'), arguments=[Number(1), Number(1)])
        )

class TestEvaluate(TestCase):
    def test_boolean(self):
        self.assertEqual(evaluate(Boolean(True)), True)
        self.assertEqual(evaluate(Boolean(False)), False)
    
    def test_number(self):
        self.assertEqual(evaluate(Number(123)), 123)
    
    def test_lambda(self):
        self.assertEqual(
            evaluate(
                Lambda(parameters=[Identifier('x')], body=Identifier('x'), environment=None)
            ),
            Lambda(parameters=[Identifier('x')], body=Identifier('x'), environment=None)
        )
    
    def test_lambda_invocation(self):
        self.assertEqual(
            evaluate(
                Invocation(operator=Lambda(parameters=[Identifier('x')], body=Identifier('x'), environment=None), arguments=[Number(1)])
            ),
            1
        )
    
    def test_builtins(self):
        self.assertEqual(
            evaluate(
                Invocation(operator=Identifier('add'), arguments=[Number(1), Number(2)])
            ),
            3
        )
    
    def test_boolean_expressions(self):
        self.assertEqual(
            evaluate(
                Invocation(operator=Identifier('eq'), arguments=[Number(1), Number(1)])
            ),
            True
        )

if __name__ == "__main__":
    main()
