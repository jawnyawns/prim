from unittest import TestCase
from prim.eval import (
    base_env,
    CallExpr,
    Closure,
    eval,
    IfExpr,
    IntLiteral,
    LambdaExpr,
    SymbolLiteral,
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
    
    def test_eval_boolean_expr(self):
        self.assertEqual(
            eval(
                CallExpr(operator=SymbolLiteral('eq'), args=[IntLiteral(1), IntLiteral(1)])
            ),
            True
        )
    
    def test_eval_if(self):
        self.assertEqual(
            eval(
                IfExpr(
                    condition=CallExpr(operator=SymbolLiteral(value='lt'),
                    args=[IntLiteral(value=1), IntLiteral(value=2)]),
                    consequent=IntLiteral(value=1), alternative=IntLiteral(value=2)
                )
            ),
            1
        )
