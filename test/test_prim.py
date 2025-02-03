from prim import (
    base_env,
    Call,
    Closure,
    eval,
    If,
    Integer,
    Lambda,
    parse,
    Symbol,
    TokenInteger,
    tokenize,
    TokenLParen,
    TokenRParen,
    TokenSymbol,
)
from unittest import (
    main,
    TestCase,
)

class TestTokenize(TestCase):
    def test_empty(self):
        self.assertEqual(tokenize(""), [])
    
    def test_invalid_character(self):
        with self.assertRaises(RuntimeError):
            tokenize("$")

    def test_symbol(self):
        self.assertEqual(tokenize("abc"), [TokenSymbol("abc")])
    
    def test_invalid_symbol(self):
        with self.assertRaises(RuntimeError):
            tokenize("(a-c)")

    def test_integer(self):
        self.assertEqual(tokenize("123"), [TokenInteger(123)])
    
    def test_negative_integer(self):
        self.assertEqual(tokenize("-123"), [TokenInteger(-123)])

    def test_invalid_integer(self):
        with self.assertRaises(RuntimeError):
            tokenize("1-2")

    def test_parentheses(self):
        self.assertEqual(
            tokenize("()"),
            [
                TokenLParen(),
                TokenRParen(),
            ]
        )

    def test_combination(self):
        self.assertEqual(
            tokenize("(lt x 5)"),
            [
                TokenLParen(),
                TokenSymbol("lt"),
                TokenSymbol("x"),
                TokenInteger(5),
                TokenRParen(),
            ]
        )

class TestParse(TestCase):
    def test_empty(self):
        with self.assertRaises(RuntimeError):
            parse([])
    
    def test_integer(self):
        self.assertEqual(parse([TokenInteger(123)]), Integer(123))
    
    def test_symbol(self):
        self.assertEqual(parse([TokenSymbol("abc")]), Symbol("abc"))

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
            Lambda(params=['x'], body=Symbol('x'))
        )
    
    def test_call(self):
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
            Call(operator=Lambda(params=['x'], body=Symbol('x')), args=[Integer(1)])
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
            Call(operator=Symbol('add'), args=[Integer(1), Integer(2)])
        )
    
    def test_boolean_expr(self):
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
            Call(operator=Symbol('eq'), args=[Integer(1), Integer(1)])
        )
    
    def test_if(self):
        self.assertEqual(
            parse(
                tokenize("(if (lt 1 2) 1 2)")
            ),
            If(
                condition=Call(operator=Symbol(value='lt'),
                args=[Integer(value=1), Integer(value=2)]),
                consequent=Integer(value=1), alternative=Integer(value=2)
            )
        )

class TestEval(TestCase):
    def test_boolean(self):
        self.assertEqual(eval(Symbol("true")), True)
        self.assertEqual(eval(Symbol("false")), False)
    
    def test_integer(self):
        self.assertEqual(eval(Integer(123)), 123)
    
    def test_lambda(self):
        self.assertEqual(
            eval(
                Lambda(params=['x'], body=Symbol('x')),
            ),
            Closure(params=['x'], body=Symbol('x'), env=base_env())
        )
    
    def test_call(self):
        self.assertEqual(
            eval(
                Call(
                    operator=Lambda(params=['x'], body=Symbol('x')),
                    args=[Integer(1)]
                )
            ),
            1
        )
    
    def test_builtins(self):
        self.assertEqual(
            eval(
                Call(operator=Symbol('add'), args=[Integer(1), Integer(2)])
            ),
            3
        )
    
    def test_boolean_expr(self):
        self.assertEqual(
            eval(
                Call(operator=Symbol('eq'), args=[Integer(1), Integer(1)])
            ),
            True
        )
    
    def test_if(self):
        self.assertEqual(
            eval(
                parse(
                    tokenize("(if (lt 1 2) 1 2)")
                )
            ),
            1
        )

if __name__ == "__main__":
    main()
