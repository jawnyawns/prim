from prim.ast import (
    CallExpr,
    IfExpr,
    IntLiteral,
    LambdaExpr,
    parse,
    SymbolLiteral,
    TokenInt,
    TokenLParen,
    TokenRParen,
    TokenSymbol,
)
from unittest import TestCase

class TestAST(TestCase):
    def test_parse_empty(self):
        with self.assertRaises(RuntimeError):
            parse([])
    
    def test_parse_integer(self):
        self.assertEqual(parse([TokenInt(123)]), IntLiteral(123))
    
    def test_parse_symbol(self):
        self.assertEqual(parse([TokenSymbol("abc")]), SymbolLiteral("abc"))

    def test_parse_lambda(self):
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
            LambdaExpr(params=['x'], body=SymbolLiteral('x'))
        )
    
    def test_parse_call(self):
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
                    TokenInt(1),
                    TokenRParen(),
                ]
            ),
            CallExpr(operator=LambdaExpr(params=['x'], body=SymbolLiteral('x')), args=[IntLiteral(1)])
        )
    
    def test_parse_builtins(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenSymbol('add'),
                    TokenInt(1),
                    TokenInt(2),
                    TokenRParen(),
                ]
            ),
            CallExpr(operator=SymbolLiteral('add'), args=[IntLiteral(1), IntLiteral(2)])
        )
    
    def test_parse_boolean_expr(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenSymbol('eq'),
                    TokenInt(1),
                    TokenInt(1),
                    TokenRParen(),
                ]
            ),
            CallExpr(operator=SymbolLiteral('eq'), args=[IntLiteral(1), IntLiteral(1)])
        )
    
    def test_parse_if(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenSymbol(value='if'),
                    TokenLParen(),
                    TokenSymbol(value='lt'),
                    TokenInt(value=1),
                    TokenInt(value=2),
                    TokenRParen(),
                    TokenInt(value=1),
                    TokenInt(value=2),
                    TokenRParen()
                ]
            ),
            IfExpr(
                condition=CallExpr(operator=SymbolLiteral(value='lt'),
                args=[IntLiteral(value=1), IntLiteral(value=2)]),
                consequent=IntLiteral(value=1), alternative=IntLiteral(value=2)
            )
        )
