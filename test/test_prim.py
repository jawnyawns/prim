from prim import (
    base_environment,
    Boolean,
    Closure,
    evaluate_expression,
    evaluate,
    Identifier,
    If,
    Integer,
    Invocation,
    parse,
    TokenInteger,
    tokenize,
    TokenLParen,
    TokenRParen,
    TokenSymbol,
)
from collections import deque
from unittest import (
    main,
    TestCase,
)

class TestTokenize(TestCase):
    def test_empty(self):
        self.assertEqual(tokenize(""), deque())
    
    def test_invalid_character(self):
        with self.assertRaises(RuntimeError):
            tokenize("$")

    def test_symbol(self):
        self.assertEqual(tokenize("abc"), deque([TokenSymbol("abc")]))
    
    def test_invalid_symbol(self):
        with self.assertRaises(RuntimeError):
            tokenize("(a-c)")

    def test_integer(self):
        self.assertEqual(tokenize("123"), deque([TokenInteger(123)]))
    
    def test_invalid_integer(self):
        with self.assertRaises(RuntimeError):
            tokenize("1-2")
    
    def test_negative_integer(self):
        self.assertEqual(tokenize("-123"), deque([TokenInteger(-123)]))

    def test_parentheses(self):
        self.assertEqual(
            tokenize("()"),
            deque([
                TokenLParen(),
                TokenRParen(),
            ])
        )

    def test_let(self):
        self.assertEqual(
            tokenize("(let x 5)"),
            deque([
                TokenLParen(),
                TokenSymbol("let"),
                TokenSymbol("x"),
                TokenInteger(5),
                TokenRParen(),
            ])
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
        expected_tokens = deque([
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
        ])
        self.assertEqual(actual_tokens, expected_tokens)

class TestParse(TestCase):
    def test_empty(self):
        self.assertEqual(parse(deque()), None)

    def test_boolean(self):
        self.assertEqual(parse(deque([TokenSymbol("true")])), Boolean(True))
        self.assertEqual(parse(deque([TokenSymbol("false")])), Boolean(False))
    
    def test_integer(self):
        self.assertEqual(parse(deque([TokenInteger(123)])), Integer(123))
    
    def test_closure(self):
        self.assertEqual(
            parse(
                deque([
                    TokenLParen(),
                    TokenSymbol('lambda'),
                    TokenLParen(),
                    TokenSymbol('x'),
                    TokenRParen(),
                    TokenSymbol('x'),
                    TokenRParen(),
                ])
            ),
            Closure(parameters=[Identifier('x')], body=Identifier('x'), environment=None)
        )
    
    def test_invocation(self):
        self.assertEqual(
            parse(
                deque([
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
                ])
            ),
            Invocation(operator=Closure(parameters=[Identifier('x')], body=Identifier('x'), environment=None), arguments=[Integer(1)])
        )
    
    def test_builtins(self):
        self.assertEqual(
            parse(
                deque([
                    TokenLParen(),
                    TokenSymbol('add'),
                    TokenInteger(1),
                    TokenInteger(2),
                    TokenRParen(),
                ])
            ),
            Invocation(operator=Identifier('add'), arguments=[Integer(1), Integer(2)])
        )
    
    def test_boolean_expressions(self):
        self.assertEqual(
            parse(
                deque([
                    TokenLParen(),
                    TokenSymbol('eq'),
                    TokenInteger(1),
                    TokenInteger(1),
                    TokenRParen(),
                ])
            ),
            Invocation(operator=Identifier('eq'), arguments=[Integer(1), Integer(1)])
        )
    
    def test_if(self):
        self.assertEqual(
            parse(
                tokenize("(if (lt 1 2) 1 2)")
            ),
            If(condition=Invocation(operator=Identifier(name='lt'), arguments=[Integer(value=1), Integer(value=2)]), consequent=Integer(value=1), alternative=Integer(value=2))
        )

class TestEvaluate(TestCase):
    def test_boolean(self):
        self.assertEqual(evaluate(Boolean(True)), True)
        self.assertEqual(evaluate(Boolean(False)), False)
    
    def test_integer(self):
        self.assertEqual(evaluate(Integer(123)), 123)
    
    def test_closure(self):
        environment = base_environment()
        self.assertEqual(
            evaluate_expression(
                Closure(parameters=[Identifier('x')], body=Identifier('x'), environment=None),
                environment
            ),
            Closure(parameters=[Identifier('x')], body=Identifier('x'), environment=environment)
        )
    
    def test_invocation(self):
        self.assertEqual(
            evaluate(
                Invocation(operator=Closure(parameters=[Identifier('x')], body=Identifier('x'), environment=None), arguments=[Integer(1)])
            ),
            1
        )
    
    def test_builtins(self):
        self.assertEqual(
            evaluate(
                Invocation(operator=Identifier('add'), arguments=[Integer(1), Integer(2)])
            ),
            3
        )
    
    def test_boolean_expressions(self):
        self.assertEqual(
            evaluate(
                Invocation(operator=Identifier('eq'), arguments=[Integer(1), Integer(1)])
            ),
            True
        )
    
    def test_if(self):
        self.assertEqual(
            evaluate(
                parse(
                    tokenize("(if (lt 1 2) 1 2)")
                )
            ),
            1
        )

if __name__ == "__main__":
    main()
