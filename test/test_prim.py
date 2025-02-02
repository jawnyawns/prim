from prim import (
    base_environment,
    Lambda,
    evaluate_expression,
    evaluate,
    Symbol,
    If,
    Integer,
    Call,
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
    
    def test_negative_integer(self):
        self.assertEqual(tokenize("-123"), deque([TokenInteger(-123)]))

    def test_invalid_integer(self):
        with self.assertRaises(RuntimeError):
            tokenize("1-2")

    def test_parentheses(self):
        self.assertEqual(
            tokenize("()"),
            deque([
                TokenLParen(),
                TokenRParen(),
            ])
        )

    def test_combination(self):
        self.assertEqual(
            tokenize("(lt x 5)"),
            deque([
                TokenLParen(),
                TokenSymbol("lt"),
                TokenSymbol("x"),
                TokenInteger(5),
                TokenRParen(),
            ])
        )

class TestParse(TestCase):
    def test_empty(self):
        with self.assertRaises(RuntimeError):
            parse(deque([]))
    
    def test_integer(self):
        self.assertEqual(parse(deque([TokenInteger(123)])), Integer(123))
    
    def test_symbol(self):
        self.assertEqual(parse(deque([TokenSymbol("abc")])), Symbol("abc"))

    def test_lambda(self):
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
            Lambda(parameters=['x'], body=Symbol('x'), environment=None)
        )
    
    def test_call(self):
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
            Call(operator=Lambda(parameters=['x'], body=Symbol('x'), environment=None), arguments=[Integer(1)])
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
            Call(operator=Symbol('add'), arguments=[Integer(1), Integer(2)])
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
            Call(operator=Symbol('eq'), arguments=[Integer(1), Integer(1)])
        )
    
    def test_if(self):
        self.assertEqual(
            parse(
                tokenize("(if (lt 1 2) 1 2)")
            ),
            If(
                condition=Call(operator=Symbol(value='lt'),
                arguments=[Integer(value=1), Integer(value=2)]),
                consequent=Integer(value=1), alternative=Integer(value=2)
            )
        )

class TestEvaluate(TestCase):
    def test_boolean(self):
        self.assertEqual(evaluate(Symbol("true")), True)
        self.assertEqual(evaluate(Symbol("false")), False)
    
    def test_integer(self):
        self.assertEqual(evaluate(Integer(123)), 123)
    
    def test_lambda(self):
        environment = base_environment()
        self.assertEqual(
            evaluate_expression(
                Lambda(parameters=['x'], body=Symbol('x'), environment=None),
                environment
            ),
            Lambda(parameters=['x'], body=Symbol('x'), environment=environment)
        )
    
    def test_call(self):
        self.assertEqual(
            evaluate(
                Call(operator=Lambda(parameters=['x'], body=Symbol('x'), environment=None), arguments=[Integer(1)])
            ),
            1
        )
    
    def test_builtins(self):
        self.assertEqual(
            evaluate(
                Call(operator=Symbol('add'), arguments=[Integer(1), Integer(2)])
            ),
            3
        )
    
    def test_boolean_expressions(self):
        self.assertEqual(
            evaluate(
                Call(operator=Symbol('eq'), arguments=[Integer(1), Integer(1)])
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
