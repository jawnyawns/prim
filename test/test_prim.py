from prim import (
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

if __name__ == "__main__":
    main()
