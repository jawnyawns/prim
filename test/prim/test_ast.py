from prim.ast import (
    CallExpr,
    IfExpr,
    FloatLiteral,
    IntLiteral,
    LambdaExpr,
    parse,
    SymbolLiteral,
    StringLiteral,
    TokenFloat,
    TokenInt,
    TokenLParen,
    TokenRParen,
    TokenSymbol,
    TokenString,
)
from unittest import TestCase

class TestAST(TestCase):
    def test_parse_empty(self):
        self.assertEqual([], parse([]))
   
    def test_parse_int(self):
        self.assertEqual(parse([TokenInt(123)]), [IntLiteral(123)])
    
    def test_parse_float(self):
        self.assertEqual(parse([TokenFloat(-123.123)]), [FloatLiteral(-123.123)])
    
    def test_parse_symbol(self):
        self.assertEqual(parse([TokenSymbol("abc")]), [SymbolLiteral("abc")])

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
            [LambdaExpr(params=['x'], body=SymbolLiteral('x'))]
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
            [CallExpr(operator=LambdaExpr(params=['x'], body=SymbolLiteral('x')), args=[IntLiteral(1)])]
        )
    
    def test_parse_builtins(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenSymbol('+'),
                    TokenInt(1),
                    TokenInt(2),
                    TokenRParen(),
                ]
            ),
            [CallExpr(operator=SymbolLiteral('+'), args=[IntLiteral(1), IntLiteral(2)])]
        )
    
    def test_parse_boolean_expr(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenSymbol('='),
                    TokenInt(1),
                    TokenInt(1),
                    TokenRParen(),
                ]
            ),
            [CallExpr(operator=SymbolLiteral('='), args=[IntLiteral(1), IntLiteral(1)])]
        )
    
    def test_parse_if(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenSymbol(value='if'),
                    TokenLParen(),
                    TokenSymbol(value='<'),
                    TokenInt(value=1),
                    TokenInt(value=2),
                    TokenRParen(),
                    TokenInt(value=1),
                    TokenInt(value=2),
                    TokenRParen()
                ]
            ),
            [
                IfExpr(
                    conditions=[
                        CallExpr(
                            operator=SymbolLiteral(value='<'),
                            args=[IntLiteral(value=1), IntLiteral(value=2)]
                        )
                    ],
                    consequents=[IntLiteral(value=1)],
                    alternative=IntLiteral(value=2)
                )
            ]
        )

    def test_parse_string(self):
        self.assertEqual(
            parse(
                [
                    TokenLParen(),
                    TokenSymbol("hello"),
                    TokenString("world"),
                    TokenString("goodbye"),
                    TokenInt(123),
                    TokenRParen(),
                ]
            ),
            [
                CallExpr(
                    operator=SymbolLiteral("hello"),
                    args=[StringLiteral("world"), StringLiteral("goodbye"), IntLiteral(123)]
                )
            ]
        )
