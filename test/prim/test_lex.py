from unittest import TestCase
from prim.lex import (
    TokenInt,
    tokenize,
    TokenLParen,
    TokenRParen,
    TokenSymbol,
    TokenString,
)

class TestLex(TestCase):
    def test_tokenize_empty(self):
        self.assertEqual(tokenize(""), [])
    
    def test_tokenize_invalid_character(self):
        with self.assertRaises(RuntimeError):
            tokenize("$")

    def test_tokenize_symbol(self):
        self.assertEqual(tokenize("abc"), [TokenSymbol("abc")])
    
    def test_tokenize_invalid_symbol(self):
        with self.assertRaises(RuntimeError):
            tokenize("(a-c)")

    def test_tokenize_integer(self):
        self.assertEqual(tokenize("123"), [TokenInt(123)])
    
    def test_tokenize_negative_integer(self):
        self.assertEqual(tokenize("-123"), [TokenInt(-123)])

    def test_tokenize_invalid_integer(self):
        with self.assertRaises(RuntimeError):
            tokenize("1-2")

    def test_tokenize_parentheses(self):
        self.assertEqual(
            tokenize("()"),
            [
                TokenLParen(),
                TokenRParen(),
            ]
        )

    def test_tokenize_combination(self):
        self.assertEqual(
            tokenize("(lt x 5)"),
            [
                TokenLParen(),
                TokenSymbol("lt"),
                TokenSymbol("x"),
                TokenInt(5),
                TokenRParen(),
            ]
        )
    
    def test_tokenize_string(self):
        self.assertEqual(
            tokenize("(hello \"world\" \"goodbye\" 123)"),
            [
                TokenLParen(),
                TokenSymbol("hello"),
                TokenString("world"),
                TokenString("goodbye"),
                TokenInt(123),
                TokenRParen(),
            ]
        )
