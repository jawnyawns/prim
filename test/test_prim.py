from prim import (
    Boolean,
    evaluate,
    Identifier,
    Invocation,
    Lambda,
    Number,
    parse,
    Token,
    tokenize,
    TokenType,
)
from unittest import (
    main,
    TestCase,
)

class TestTokenize(TestCase):
    def test_empty(self):
        self.assertEqual(tokenize(""), [])

    def test_symbol(self):
        self.assertEqual(tokenize("x"), [Token(TokenType.SYMBOL, "x")])

    def test_number(self):
        self.assertEqual(tokenize("123"), [Token(TokenType.NUMBER, "123")])

    def test_parentheses(self):
        self.assertEqual(
            tokenize("()"),
            [
                Token(TokenType.LPAREN, "("),
                Token(TokenType.RPAREN, ")"),
            ]
        )

    def test_let(self):
        self.assertEqual(
            tokenize("(let x 5)"),
            [
                Token(TokenType.LPAREN, "("),
                Token(TokenType.SYMBOL, "let"),
                Token(TokenType.SYMBOL, "x"),
                Token(TokenType.NUMBER, "5"),
                Token(TokenType.RPAREN, ")"),
            ]
        )

    def test_complex_example(self):
        source_code = """(
  let (
    (factorial (lambda (n) (if (equals n 0) 1 (multiply n (factorial (subtract n 1))))))
    (n 5)
  )
  (factorial n)
)
"""
        actual_tokens = tokenize(source_code)
        expected_tokens = [
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "let"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "factorial"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "lambda"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "n"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "if"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "equals"),
            Token(TokenType.SYMBOL, "n"),
            Token(TokenType.NUMBER, "0"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.NUMBER, "1"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "multiply"),
            Token(TokenType.SYMBOL, "n"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "factorial"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "subtract"),
            Token(TokenType.SYMBOL, "n"),
            Token(TokenType.NUMBER, "1"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "n"),
            Token(TokenType.NUMBER, "5"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.SYMBOL, "factorial"),
            Token(TokenType.SYMBOL, "n"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
        ]
        self.assertEqual(actual_tokens, expected_tokens)

class TestParse(TestCase):
    def test_empty(self):
        self.assertEqual(parse([]), None)

    def test_boolean(self):
        self.assertEqual(parse([Token(TokenType.SYMBOL, "true")]), Boolean(True))
        self.assertEqual(parse([Token(TokenType.SYMBOL, "false")]), Boolean(False))
    
    def test_number(self):
        self.assertEqual(parse([Token(TokenType.NUMBER, 123)]), Number(123))
    
    def test_lambda(self):
        self.assertEqual(
            parse(
                [
                    Token(TokenType.LPAREN, '('),
                    Token(TokenType.SYMBOL, 'lambda'),
                    Token(TokenType.LPAREN, '('),
                    Token(TokenType.SYMBOL, 'x'),
                    Token(TokenType.RPAREN, ')'),
                    Token(TokenType.SYMBOL, 'x'),
                    Token(TokenType.RPAREN, ')'),
                ]
            ),
            Lambda(parameters=[Identifier('x')], body=Identifier('x'), environment={})
        )
    
    def test_lambda_invocation(self):
        self.assertEqual(
            parse(
                [
                    Token(TokenType.LPAREN, '('),
                    Token(TokenType.LPAREN, '('),
                    Token(TokenType.SYMBOL, 'lambda'),
                    Token(TokenType.LPAREN, '('),
                    Token(TokenType.SYMBOL, 'x'),
                    Token(TokenType.RPAREN, ')'),
                    Token(TokenType.SYMBOL, 'x'),
                    Token(TokenType.RPAREN, ')'),
                    Token(TokenType.NUMBER, '1'),
                    Token(TokenType.RPAREN, ')'),
                ]
            ),
            Invocation(operator=Lambda(parameters=[Identifier('x')], body=Identifier('x'), environment={}), arguments=[Number(1)])
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
                Lambda(parameters=[Identifier('x')], body=Identifier('x'), environment={})
            ),
            Lambda(parameters=[Identifier('x')], body=Identifier('x'), environment={})
        )
    
    def test_lambda_invocation(self):
        self.assertEqual(
            evaluate(
                Invocation(operator=Lambda(parameters=[Identifier('x')], body=Identifier('x'), environment={}), arguments=[Number(1)])
            ),
            1
        )

if __name__ == "__main__":
    main()
