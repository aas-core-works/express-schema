"""Tokenize Express schema."""

import enum
import re
from typing import Final, cast, List, Tuple, Optional

from icontract import require

from express_schema._common import NonNegativeInt


class TokenKind(enum.Enum):
    """
    Represent the kind of the token.

    We did not want to clutter the context with too many types, but rather use token
    kind to distinguish the semantics between the tokens.
    """

    # Comments
    COMMENT = "COMMENT"

    # Keywords (case-insensitive)
    SELF = "SELF"
    WHERE = "WHERE"
    ENTITY = "ENTITY"
    END_ENTITY = "END_ENTITY"
    TYPE = "TYPE"
    END_TYPE = "END_TYPE"
    FUNCTION = "FUNCTION"
    END_FUNCTION = "END_FUNCTION"
    PROCEDURE = "PROCEDURE"
    END_PROCEDURE = "END_PROCEDURE"
    RULE = "RULE"
    END_RULE = "END_RULE"
    SUBTYPE = "SUBTYPE"
    SUPERTYPE = "SUPERTYPE"
    OF = "OF"
    DERIVE = "DERIVE"
    INVERSE = "INVERSE"
    OPTIONAL = "OPTIONAL"
    UNIQUE = "UNIQUE"
    ABSTRACT = "ABSTRACT"
    SET = "SET"
    LIST = "LIST"
    ARRAY = "ARRAY"
    BAG = "BAG"
    BOOLEAN = "BOOLEAN"
    INTEGER = "INTEGER"
    REAL = "REAL"
    STRING = "STRING"
    NUMBER = "NUMBER"
    LOGICAL = "LOGICAL"
    BINARY = "BINARY"
    GENERIC = "GENERIC"
    FIXED = "FIXED"
    TRUE = "TRUE"
    FALSE = "FALSE"
    UNKNOWN = "UNKNOWN"
    ANDOR = "ANDOR"
    AND = "AND"
    OR = "OR"
    XOR = "XOR"
    NOT = "NOT"
    IF = "IF"
    THEN = "THEN"
    ELSE = "ELSE"
    END_IF = "END_IF"
    CASE = "CASE"
    END_CASE = "END_CASE"
    FORALL = "FORALL"
    RETURN = "RETURN"
    SCHEMA = "SCHEMA"
    END_SCHEMA = "END_SCHEMA"
    USE = "USE"
    REFERENCE = "REFERENCE"
    FROM = "FROM"
    ALIAS = "ALIAS"
    END_ALIAS = "END_ALIAS"
    AS = "AS"
    IN = "IN"
    LIKE = "LIKE"
    AGGREGATE = "AGGREGATE"
    BY = "BY"
    DIV = "DIV"
    MOD = "MOD"
    CONSTANT = "CONSTANT"
    END_CONSTANT = "END_CONSTANT"
    ENUMERATION = "ENUMERATION"
    FOR = "FOR"
    LOCAL = "LOCAL"
    END_LOCAL = "END_LOCAL"
    ONEOF = "ONEOF"
    OTHERWISE = "OTHERWISE"
    QUERY = "QUERY"
    SELECT = "SELECT"
    TO = "TO"
    REPEAT = "REPEAT"
    END_REPEAT = "END_REPEAT"
    WHILE = "WHILE"
    UNTIL = "UNTIL"
    BEGIN = "BEGIN"
    END = "END"
    ESCAPE = "ESCAPE"
    SKIP = "SKIP"
    VAR = "VAR"

    # Comparison
    NE = "NE"
    LE = "LE"
    GE = "GE"
    LT = "LT"
    GT = "GT"
    COLON_LT_GT_COLON = "COLON_LT_GT_COLON"
    COLON_EQ_COLON = "COLON_EQ_COLON"

    # Misc. operators
    LT_STAR = "LT_STAR"
    DOUBLE_PIPE = "DOUBLE_PIPE"
    COLON_EQ = "COLON_EQ"
    DOUBLE_HYPHEN = "DOUBLE_HYPHEN"

    # Arithmetic
    DOUBLE_STAR = "DOUBLE_STAR"
    PLUS = "PLUS"
    MINUS = "MINUS"
    STAR = "STAR"
    SLASH = "SLASH"

    # Punctuation (include backslash)
    BACKSLASH = "BACKSLASH"
    DOT = "DOT"
    LPAR = "LPAR"
    RPAR = "RPAR"
    LSQ = "LSQ"
    RSQ = "RSQ"
    LCRLY = "LCRLY"
    RCRLY = "RCRLY"
    PIPE = "PIPE"
    COMMA = "COMMA"
    SEMI = "SEMI"
    COLON = "COLON"
    EQ = "EQ"

    IDENTIFIER = "IDENTIFIER"

    BINARY_LITERAL = "BINARY_LITERAL"
    INTEGER_LITERAL = "INTEGER_LITERAL"
    REAL_LITERAL = "REAL_LITERAL"
    STRING_LITERAL = "STRING"

    QMARK = "QMARK"


class Position:
    """Represent a position in a source code."""

    lineno: Final[NonNegativeInt]
    column: Final[NonNegativeInt]

    def __init__(self, lineno: NonNegativeInt, column: NonNegativeInt) -> None:
        self.lineno = lineno
        self.column = column

    def __str__(self) -> str:
        return f"{int(self.lineno) + 1}:{int(self.column) + 1}"

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Position):
            return NotImplemented

        return self.lineno == other.lineno and self.column == other.column


class Token:
    """Model a token in the tokenization stream."""

    kind: Final[TokenKind]
    text: Final[str]
    start: Final[NonNegativeInt]
    position: Final[Position]

    def __init__(
        self,
        kind: TokenKind,
        text: str,
        #: Index in the original text
        start: NonNegativeInt,
        #: Line number and column in the original text
        position: Position,
    ) -> None:
        self.kind = kind
        self.text = text
        self.start = start
        self.position = position

    def __str__(self) -> str:
        return f"<_Token {self.kind.name} at {self.position} {self.text!r}>"


class _TokenizationRule:
    pattern: Final[re.Pattern[str]]
    kind: Final[TokenKind]

    def __init__(self, pattern: re.Pattern[str], kind: TokenKind) -> None:
        self.pattern = pattern
        self.kind = kind


# noinspection RegExpSimplifiable
IDENTIFIER_RE = re.compile(r"^[a-zA-Z_][a-zA-Z_0-9]*$")


class Identifier(str):
    """Represent an identifier."""

    @require(lambda value: IDENTIFIER_RE.fullmatch(value))
    def __new__(cls, value: str) -> "Identifier":
        return cast(Identifier, value)


def _create_tokenization_rules() -> List[_TokenizationRule]:
    @require(lambda word: IDENTIFIER_RE.fullmatch(word))
    def kw(word: str, kind: TokenKind) -> _TokenizationRule:
        # case-insensitive keyword bounded by word boundaries
        return _TokenizationRule(re.compile(rf"(?i)\b{word}\b"), kind)

    def rx(pat: str, kind: TokenKind) -> _TokenizationRule:
        return _TokenizationRule(re.compile(pat), kind)

    # noinspection PyListCreation
    rules: List[_TokenizationRule] = []

    # --- Comment ---
    rules.append(
        _TokenizationRule(
            # NOTE (mristin):
            # We make the regular expression non-greedy with `?` after the `.*` so that
            # we do not consume the content between two content blocks.
            pattern=re.compile(r"\(\*.*?\*\)", re.DOTALL),
            kind=TokenKind.COMMENT,
        )
    )

    # --- Literals ---
    rules.append(rx(r"[0-9]+\.[0-9]*([Ee][+-]?[0-9]+)?", TokenKind.REAL_LITERAL))
    rules.append(rx(r"\.[0-9]+([Ee][+-]?[0-9]+)?", TokenKind.REAL_LITERAL))
    rules.append(rx(r"[0-9]+\.", TokenKind.REAL_LITERAL))
    rules.append(rx(r"[0-9]+", TokenKind.INTEGER_LITERAL))

    rules.append(rx(r"'((''|\\.|[^'])*?)'", TokenKind.STRING_LITERAL))

    rules.append(rx(r"%[01][01]*", TokenKind.BINARY_LITERAL))

    # --- Keywords (case-insensitive) ---
    rules.extend(
        [
            kw("SELF", TokenKind.SELF),
            kw("WHERE", TokenKind.WHERE),
            kw("ENTITY", TokenKind.ENTITY),
            kw("END_ENTITY", TokenKind.END_ENTITY),
            kw("TYPE", TokenKind.TYPE),
            kw("END_TYPE", TokenKind.END_TYPE),
            kw("FUNCTION", TokenKind.FUNCTION),
            kw("END_FUNCTION", TokenKind.END_FUNCTION),
            kw("PROCEDURE", TokenKind.PROCEDURE),
            kw("END_PROCEDURE", TokenKind.END_PROCEDURE),
            kw("RULE", TokenKind.RULE),
            kw("END_RULE", TokenKind.END_RULE),
            kw("SUBTYPE", TokenKind.SUBTYPE),
            kw("SUPERTYPE", TokenKind.SUPERTYPE),
            kw("OF", TokenKind.OF),
            kw("DERIVE", TokenKind.DERIVE),
            kw("INVERSE", TokenKind.INVERSE),
            kw("OPTIONAL", TokenKind.OPTIONAL),
            kw("UNIQUE", TokenKind.UNIQUE),
            kw("ABSTRACT", TokenKind.ABSTRACT),
            kw("SET", TokenKind.SET),
            kw("LIST", TokenKind.LIST),
            kw("ARRAY", TokenKind.ARRAY),
            kw("BAG", TokenKind.BAG),
            kw("BOOLEAN", TokenKind.BOOLEAN),
            kw("INTEGER", TokenKind.INTEGER),
            kw("REAL", TokenKind.REAL),
            kw("STRING", TokenKind.STRING),
            kw("NUMBER", TokenKind.NUMBER),
            kw("LOGICAL", TokenKind.LOGICAL),
            kw("BINARY", TokenKind.BINARY),
            kw("GENERIC", TokenKind.GENERIC),
            kw("FIXED", TokenKind.FIXED),
            kw("TRUE", TokenKind.TRUE),
            kw("FALSE", TokenKind.FALSE),
            kw("UNKNOWN", TokenKind.UNKNOWN),
            kw("ANDOR", TokenKind.ANDOR),
            kw("AND", TokenKind.AND),
            kw("OR", TokenKind.OR),
            kw("XOR", TokenKind.XOR),
            kw("NOT", TokenKind.NOT),
            kw("IF", TokenKind.IF),
            kw("THEN", TokenKind.THEN),
            kw("ELSE", TokenKind.ELSE),
            kw("END_IF", TokenKind.END_IF),
            kw("CASE", TokenKind.CASE),
            kw("END_CASE", TokenKind.END_CASE),
            kw("FORALL", TokenKind.FORALL),
            kw("RETURN", TokenKind.RETURN),
            kw("SCHEMA", TokenKind.SCHEMA),
            kw("END_SCHEMA", TokenKind.END_SCHEMA),
            kw("USE", TokenKind.USE),
            kw("REFERENCE", TokenKind.REFERENCE),
            kw("FROM", TokenKind.FROM),
            kw("ALIAS", TokenKind.ALIAS),
            kw("END_ALIAS", TokenKind.END_ALIAS),
            kw("AS", TokenKind.AS),
            kw("IN", TokenKind.IN),
            kw("LIKE", TokenKind.LIKE),
            kw("AGGREGATE", TokenKind.AGGREGATE),
            kw("BY", TokenKind.BY),
            kw("DIV", TokenKind.DIV),
            kw("MOD", TokenKind.MOD),
            kw("CONSTANT", TokenKind.CONSTANT),
            kw("END_CONSTANT", TokenKind.END_CONSTANT),
            kw("ENUMERATION", TokenKind.ENUMERATION),
            kw("FOR", TokenKind.FOR),
            kw("LOCAL", TokenKind.LOCAL),
            kw("END_LOCAL", TokenKind.END_LOCAL),
            kw("ONEOF", TokenKind.ONEOF),
            kw("OTHERWISE", TokenKind.OTHERWISE),
            kw("QUERY", TokenKind.QUERY),
            kw("SELECT", TokenKind.SELECT),
            kw("TO", TokenKind.TO),
            kw("REPEAT", TokenKind.REPEAT),
            kw("END_REPEAT", TokenKind.END_REPEAT),
            kw("WHILE", TokenKind.WHILE),
            kw("UNTIL", TokenKind.UNTIL),
            kw("BEGIN", TokenKind.BEGIN),
            kw("END", TokenKind.END),
            kw("ESCAPE", TokenKind.ESCAPE),
            kw("SKIP", TokenKind.SKIP),
            kw("VAR", TokenKind.VAR),
        ]
    )

    # --- Misc. operators ---
    # NOTE (mristin):
    # This must come before LT token!
    rules.append(rx(r"<\*", TokenKind.LT_STAR))
    rules.append(rx(r"\|\|", TokenKind.DOUBLE_PIPE))
    rules.append(rx(r"--", TokenKind.DOUBLE_HYPHEN))

    # --- Comparison (order matters: two-character before one-character) ---
    rules.extend(
        [
            rx(r":<>:", TokenKind.COLON_LT_GT_COLON),
            rx(r":=:", TokenKind.COLON_EQ_COLON),
            rx(r"<>", TokenKind.NE),
            rx(r"<=", TokenKind.LE),
            rx(r">=", TokenKind.GE),
            rx(r"<", TokenKind.LT),
            rx(r">", TokenKind.GT),
        ]
    )

    # --- Misc. operators ---
    rules.append(rx(r":=", TokenKind.COLON_EQ))

    # --- Arithmetic ---
    rules.extend(
        [
            rx(r"\*\*", TokenKind.DOUBLE_STAR),
            rx(r"\+", TokenKind.PLUS),
            rx(r"-", TokenKind.MINUS),
            rx(r"\*", TokenKind.STAR),
            rx(r"/", TokenKind.SLASH),
        ]
    )

    # --- Punctuation (include backslash) ---
    rules.extend(
        [
            rx(r"\\", TokenKind.BACKSLASH),
            rx(r"\.", TokenKind.DOT),
            rx(r"\(", TokenKind.LPAR),
            rx(r"\)", TokenKind.RPAR),
            rx(r"\[", TokenKind.LSQ),
            rx(r"\]", TokenKind.RSQ),
            rx(r"{", TokenKind.LCRLY),
            rx(r"}", TokenKind.RCRLY),
            rx(r"\|", TokenKind.PIPE),
            rx(r",", TokenKind.COMMA),
            rx(r";", TokenKind.SEMI),
            rx(r":", TokenKind.COLON),
            rx(r"=", TokenKind.EQ),
        ]
    )

    rules.append(rx(r"[A-Za-z_][A-Za-z0-9_]*", TokenKind.IDENTIFIER))

    rules.append(rx(r"\?", TokenKind.QMARK))

    return rules


_TOKENIZATION_RULES = _create_tokenization_rules()

_WHITESPACE_RE = re.compile(r"\s+")


def lex(text: str) -> Tuple[List[Token], bool]:
    """
    Lex the given text.

    Return the successfully lexed tokens and success/fail.
    """
    tokens = []  # type: List[Token]

    # NOTE (mristin):
    # Theoretically, we could use range trees, but since we are not going to parse
    # huge texts, this is a quick work-around.
    linenos = []
    columns = []
    current_lineno = 0
    current_column = 0
    for character in text:
        linenos.append(current_lineno)
        columns.append(current_column)

        if character == "\n":
            current_lineno += 1
            current_column = 0
        else:
            current_column += 1

    start = 0
    while start < len(text):
        old_start = start

        match: Optional[re.Match[str]] = None
        for rule in _TOKENIZATION_RULES:
            match = rule.pattern.match(text, pos=start)

            if match is not None:
                assert len(match.group(0)) > 0, (
                    f"Empty strings should never be matched against tokenization rule, "
                    f"but rule pattern matched: {rule.pattern}"
                )

                token = Token(
                    kind=rule.kind,
                    text=match.group(0),
                    start=NonNegativeInt(start),
                    position=Position(
                        lineno=NonNegativeInt(linenos[start]),
                        column=NonNegativeInt(columns[start]),
                    ),
                )

                tokens.append(token)
                start = match.end()
                break

        if match is None:
            # NOTE (mristin):
            # We ignore the whitespace as it is irrelevant for us.
            match = _WHITESPACE_RE.match(text, pos=start)
            if match is not None:
                start = match.end()

        if match is None:
            return tokens, False

        assert start > old_start, (
            f"Loop invariant: {start=}, {old_start=}, "
            f"{len(text)=}, {text[start:start+10]=}, "
            f"{match=}"
        )

    return tokens, True
