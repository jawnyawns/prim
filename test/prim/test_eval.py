from unittest import TestCase
from prim.eval import (
    base_env,
    CallExpr,
    Closure,
    DefineExpr,
    eval,
    FloatLiteral,
    IfExpr,
    IntLiteral,
    LambdaExpr,
    ListEmpty,
    ListNode,
    StringLiteral,
    SymbolLiteral,
)

class TestEval(TestCase):
    def test_eval_boolean(self):
        self.assertEqual(eval([SymbolLiteral("true")]), [True])
        self.assertEqual(eval([SymbolLiteral("false")]), [False])
    
    def test_eval_integer(self):
        self.assertEqual(eval([IntLiteral(123)]), [123])
    
    def test_eval_lambda(self):
        self.assertEqual(
            eval([
                LambdaExpr(params=['x'], body=SymbolLiteral('x')),
            ]),
            [Closure(params=['x'], body=SymbolLiteral('x'), env=base_env())]
        )
    
    def test_eval_call(self):
        self.assertEqual(
            eval([
                CallExpr(
                    operator=LambdaExpr(params=['x'], body=SymbolLiteral('x')),
                    args=[IntLiteral(1)]
                )
            ]),
            [1]
        )
    
    def test_eval_builtins(self):
        self.assertEqual(
            eval([
                CallExpr(operator=SymbolLiteral('+'), args=[IntLiteral(1), IntLiteral(2)])
            ]),
            [3]
        )
    
    def test_eval_builtins_complex_math(self):
        self.assertEqual(
            eval([
                CallExpr(
                    operator=SymbolLiteral(value='+'),
                    args=[
                        IntLiteral(value=1),
                        CallExpr(
                            operator=SymbolLiteral(value='*'),
                            args=[
                                CallExpr(
                                    operator=SymbolLiteral(value='-'),
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
            ]),
            [-19.96]
        )
    
    def test_eval_boolean_expr_with_ints(self):
        self.assertEqual(
            eval([
                CallExpr(operator=SymbolLiteral('='), args=[IntLiteral(1), IntLiteral(1)])
            ]),
            [True]
        )
    
    def test_eval_and(self):
        self.assertEqual(
            eval([
                CallExpr(operator=SymbolLiteral('and'), args=[SymbolLiteral("true"), SymbolLiteral("true")])
            ]),
            [True]
        )
    
    def test_eval_or(self):
        self.assertEqual(
            eval([
                CallExpr(operator=SymbolLiteral('or'), args=[SymbolLiteral("true"), SymbolLiteral("false")])
            ]),
            [True]
        )
    
    def test_eval_not(self):
        self.assertEqual(
            eval([
                CallExpr(operator=SymbolLiteral('not'), args=[SymbolLiteral("false")])
            ]),
            [True]
        )
    
    def test_eval_if(self):
        self.assertEqual(
            eval([
                IfExpr(
                    conditions=[
                        CallExpr(
                            operator=SymbolLiteral(value='>'),
                            args=[IntLiteral(value=1), IntLiteral(value=2)]
                        ),
                        CallExpr(
                            operator=SymbolLiteral(value='<'),
                            args=[IntLiteral(value=1), IntLiteral(value=2)]
                        )
                    ],
                    consequents=[IntLiteral(value=1), IntLiteral(value=2)],
                    alternative=IntLiteral(value=3)
                )
            ]),
            [2]
        )

    def test_eval_string(self):
        self.assertEqual(
            eval([
                CallExpr(
                    operator=LambdaExpr(params=['x'], body=SymbolLiteral('x')),
                    args=[StringLiteral("hey")]
                )
            ]),
            ["hey"]
        )

    def test_eval_empty_list(self):
        self.assertEqual(
            eval([
                CallExpr(
                    operator=SymbolLiteral(value="list"),
                    args=[]
                )
            ]),
            [ListEmpty()]
        )
    
    def test_eval_list_prepend(self):
        self.assertEqual(
            [ListNode(1, ListEmpty())],
            eval([
                CallExpr(
                    operator=SymbolLiteral(value="::"),
                    args=[
                        IntLiteral(1),
                        CallExpr(
                            operator=SymbolLiteral(value="list"),
                            args=[]
                        )
                    ]
                )
            ])
        )
    
    def test_eval_list_get_value(self):
        self.assertEqual(
            [1],
            eval([
                CallExpr(
                    operator=SymbolLiteral("value"),
                    args=[
                        CallExpr(
                            operator=SymbolLiteral(value="::"),
                            args=[
                                IntLiteral(1),
                                CallExpr(
                                    operator=SymbolLiteral(value="list"),
                                    args=[]
                                )
                            ]
                        )
                    ]
                )
            ])
        )

    def test_eval_define(self):
        self.assertEqual(
            ['<ENV MODIFIED>', 9],
            eval([
                DefineExpr(
                    name='f',
                    body=LambdaExpr(
                        params=['x', 'y', 'z'],
                        body=CallExpr(
                            operator=SymbolLiteral(value='*'),
                            args=[
                                SymbolLiteral(value='z'),
                                CallExpr(
                                    operator=SymbolLiteral(value='+'),
                                    args=[SymbolLiteral(value='x'), SymbolLiteral(value='y')]
                                )
                            ]
                        )
                    )
                ),
                CallExpr(
                    operator=SymbolLiteral(value='f'),
                    args=[IntLiteral(value=1), IntLiteral(value=2), IntLiteral(value=3)]
                ),
            ])
        )
