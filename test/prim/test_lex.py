from unittest import TestCase
from prim.lex import (
    TokenFloat,
    TokenInt,
    tokenize,
    TokenLParen,
    TokenRParen,
    TokenString,
    TokenSymbol,
)

class TestLex(TestCase):
    def test_tokenize_empty(self):
        self.assertEqual(tokenize(""), [])
    
    def test_tokenize_invalid_character(self):
        with self.assertRaises(RuntimeError):
            tokenize("$")

    def test_tokenize_symbol(self):
        self.assertEqual(tokenize("abc"), [TokenSymbol("abc")])
    
    def test_tokenize_symbol_unusual(self):
        self.assertEqual(tokenize("_<>=+-*/?:"), [TokenSymbol("_<>=+-*/?:")])
    
    def test_tokenize_invalid_symbol(self):
        with self.assertRaises(RuntimeError):
            tokenize("abc%")

    def test_tokenize_int(self):
        self.assertEqual(tokenize("123"), [TokenInt(123)])
    
    def test_tokenize_negative_int(self):
        self.assertEqual(tokenize("-123"), [TokenInt(-123)])

    def test_tokenize_invalid_int_bad_interior_char(self):
        with self.assertRaises(RuntimeError):
            tokenize("1-2")
    
    def test_tokenize_float(self):
        self.assertEqual(tokenize("123.123"), [TokenFloat(123.123)])

    def test_tokenize_negative_float(self):
        self.assertEqual(tokenize("-123.0"), [TokenFloat(-123.0)])
    
    def test_tokenize_invalid_float_no_suffix(self):
        with self.assertRaises(RuntimeError):
            tokenize("12.")

    def test_tokenize_invalid_float_no_prefix(self):
        with self.assertRaises(RuntimeError):
            tokenize(".123")

    def test_tokenize_invalid_float_invalid_prefix(self):
        with self.assertRaises(RuntimeError):
            tokenize("-.123")

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
            tokenize("(< x 5)"),
            [
                TokenLParen(),
                TokenSymbol("<"),
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
