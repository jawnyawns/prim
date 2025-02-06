import string

from dataclasses import dataclass

### TOKENS ###

@dataclass(frozen=True)
class Token:
    pass

@dataclass(frozen=True)
class TokenLParen(Token):
    pass

@dataclass(frozen=True)
class TokenRParen(Token):
    pass

@dataclass(frozen=True)
class TokenNonParen(Token):
    pass

@dataclass(frozen=True)
class TokenInt(TokenNonParen):
    value: int

@dataclass(frozen=True)
class TokenSymbol(TokenNonParen):
    """
    A symbol is a sequence of characters that can represent many things:
    - The name of an identifier
    - The name of a built-in operator
    - The textual representation of a literal
    - A reserved keyword
    """
    value: str

@dataclass(frozen=True)
class TokenString(TokenNonParen):
    value: str

### TOKENIZATION ###

_LPAREN_CHARS = frozenset("(")
_RPAREN_CHARS = frozenset(")")
_SPACE_CHARS = frozenset(string.whitespace)
_INTEGER_START_CHARS = frozenset(string.digits + "-")
_INTEGER_REST_CHARS = frozenset(string.digits)
_INTEGER_END_CHARS = frozenset(string.whitespace + "()")
_SYMBOL_START_CHARS = frozenset(string.ascii_lowercase)
_SYMBOL_REST_CHARS = frozenset(string.ascii_lowercase + string.digits + "_")
_SYMBOL_END_CHARS = frozenset(string.whitespace + "()")
_STRING_DELIMITER_CHARS = frozenset("\"")

def tokenize(source_code: str) -> list[Token]:
    tokens, _ = _tokenize_helper(source_code, [])
    return tokens

def _tokenize_helper(source_code: str, tokens: list[Token]) -> tuple[list[Token], str]:
    if not source_code:
        return tokens, source_code
    ch, rest = source_code[0], source_code[1:]
    if ch in _SPACE_CHARS:
        return _tokenize_helper(rest, tokens)
    elif ch in _LPAREN_CHARS:
        return _tokenize_helper(rest, tokens + [TokenLParen()])
    elif ch in _RPAREN_CHARS:
        return _tokenize_helper(rest, tokens + [TokenRParen()])
    elif ch in _INTEGER_START_CHARS:
        consumed, remaining = _consume_until_delimiter(source_code, _INTEGER_END_CHARS)
        if _is_valid_integer(consumed):
            return _tokenize_helper(remaining, tokens + [TokenInt(value=int(consumed))])
        else:
            raise RuntimeError(f"Invalid integer '{consumed}'")
    elif ch in _SYMBOL_START_CHARS:
        consumed, remaining = _consume_until_delimiter(source_code, _SYMBOL_END_CHARS)
        if _is_valid_symbol(consumed):
            return _tokenize_helper(remaining, tokens + [TokenSymbol(value=consumed)])
        else:
            raise RuntimeError(f"Invalid symbol '{consumed}'")
    elif ch in _STRING_DELIMITER_CHARS:
        skip_start_quote = rest
        consumed, remaining = _consume_until_delimiter(skip_start_quote, _STRING_DELIMITER_CHARS)
        skip_end_quote = remaining[1:]
        return _tokenize_helper(skip_end_quote, tokens + [TokenString(value=consumed)])
    else:
        raise RuntimeError(f"Unexpected character '{ch}'")

def _consume_until_delimiter(source_code: str, end_delimiters: frozenset[str]) -> tuple[str, str]:
    generator = (i for i, ch in enumerate(source_code) if ch in end_delimiters)
    end = next(generator, len(source_code))
    return source_code[:end], source_code[end:]

def _is_valid_integer(text: str) -> bool:
    return text[0] in _INTEGER_START_CHARS and all(c in _INTEGER_REST_CHARS for c in text[1:]) if text else False

def _is_valid_symbol(text: str) -> bool:
    return text[0] in _SYMBOL_START_CHARS and all(c in _SYMBOL_REST_CHARS for c in text[1:]) if text else False
