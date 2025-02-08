from unittest import TestCase
from prim.eval import (
    base_env,
    CallExpr,
    Closure,
    eval,
    IfExpr,
    FloatLiteral,
    IntLiteral,
    LambdaExpr,
    SymbolLiteral,
    StringLiteral,
)

class TestEval(TestCase):
    def test_eval_boolean(self):
        self.assertEqual(eval(SymbolLiteral("true")), True)
        self.assertEqual(eval(SymbolLiteral("false")), False)
    
    def test_eval_integer(self):
        self.assertEqual(eval(IntLiteral(123)), 123)
    
    def test_eval_lambda(self):
        self.assertEqual(
            eval(
                LambdaExpr(params=['x'], body=SymbolLiteral('x')),
            ),
            Closure(params=['x'], body=SymbolLiteral('x'), env=base_env())
        )
    
    def test_eval_call(self):
        self.assertEqual(
            eval(
                CallExpr(
                    operator=LambdaExpr(params=['x'], body=SymbolLiteral('x')),
                    args=[IntLiteral(1)]
                )
            ),
            1
        )
    
    def test_eval_builtins(self):
        self.assertEqual(
            eval(
                CallExpr(operator=SymbolLiteral('add'), args=[IntLiteral(1), IntLiteral(2)])
            ),
            3
        )
    
    def test_eval_builtins_complex_math(self):
        self.assertEqual(
            eval(
                CallExpr(
                    operator=SymbolLiteral(value='add'),
                    args=[
                        IntLiteral(value=1),
                        CallExpr(
                            operator=SymbolLiteral(value='mul'),
                            args=[
                                CallExpr(
                                    operator=SymbolLiteral(value='sub'),
                                    args=[
                                        FloatLiteral(value=-2.1),
                                        FloatLiteral(value=3.14)
                                    ]
                                ),
                                IntLiteral(value=4)
                            ]
                        )
                    ]
                )
            ),
            -19.96
        )
    
    def test_eval_boolean_expr_with_ints(self):
        self.assertEqual(
            eval(
                CallExpr(operator=SymbolLiteral('eq'), args=[IntLiteral(1), IntLiteral(1)])
            ),
            True
        )
    
    def test_eval_and(self):
        self.assertEqual(
            eval(
                CallExpr(operator=SymbolLiteral('and'), args=[SymbolLiteral("true"), SymbolLiteral("true")])
            ),
            True
        )
    
    def test_eval_or(self):
        self.assertEqual(
            eval(
                CallExpr(operator=SymbolLiteral('or'), args=[SymbolLiteral("true"), SymbolLiteral("false")])
            ),
            True
        )
    
    def test_eval_not(self):
        self.assertEqual(
            eval(
                CallExpr(operator=SymbolLiteral('not'), args=[SymbolLiteral("false")])
            ),
            True
        )
    
    def test_eval_if(self):
        self.assertEqual(
            eval(
                IfExpr(
                    conditions=[
                        CallExpr(
                            operator=SymbolLiteral(value='gt'),
                            args=[IntLiteral(value=1), IntLiteral(value=2)]
                        ),
                        CallExpr(
                            operator=SymbolLiteral(value='lt'),
                            args=[IntLiteral(value=1), IntLiteral(value=2)]
                        )
                    ],
                    consequents=[IntLiteral(value=1), IntLiteral(value=2)],
                    alternative=IntLiteral(value=3)
                )
            ),
            2
        )

    def test_eval_string(self):
        self.assertEqual(
            eval(
                CallExpr(
                    operator=LambdaExpr(params=['x'], body=SymbolLiteral('x')),
                    args=[StringLiteral("hey")]
                )
            ),
            "hey"
        )
