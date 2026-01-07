# pylint: disable=missing-docstring

import unittest
from typing import Optional, Sequence

import tests.common

from express_schema import lex


class TestLex(unittest.TestCase):
    # pylint: disable=protected-access

    def assert_token_kinds(
        self,
        tokens: Sequence[lex.Token],
        expected: Sequence[lex.TokenKind],
        message: Optional[str] = None,
    ) -> None:
        self.assertSequenceEqual(expected, [token.kind for token in tokens], message)

    def test_empty(self) -> None:
        tokens, ok = lex.lex("")
        assert ok
        self.assertEqual([], tokens)

    def test_keywords_case_insensitive(self) -> None:
        tokens, ok = lex.lex("entity Wall; end_entity;")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.ENTITY,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.SEMI,
                lex.TokenKind.END_ENTITY,
                lex.TokenKind.SEMI,
            ],
        )

    def test_keyword_boundaries(self) -> None:
        # Keywords should not match as substrings inside identifiers.
        tokens, ok = lex.lex("ANDY ORBIT END_ENTITYX iffy thenx")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.IDENTIFIER,
            ],
        )

    def test_schema_header_footer(self) -> None:
        tokens, ok = lex.lex("SCHEMA Ifc4x3; END_SCHEMA;")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.SCHEMA,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.SEMI,
                lex.TokenKind.END_SCHEMA,
                lex.TokenKind.SEMI,
            ],
        )

    def test_self_backslash_qualification(self) -> None:
        tokens, ok = lex.lex(r"WHERE SELF\IfcRoot.GlobalId = 'abc';")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.WHERE,
                lex.TokenKind.SELF,
                lex.TokenKind.BACKSLASH,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.DOT,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.EQ,
                lex.TokenKind.STRING_LITERAL,
                lex.TokenKind.SEMI,
            ],
        )

    def test_backslash_inside_string_is_not_tokenized(self) -> None:
        tokens, ok = lex.lex(r"'A\X2\00E9\X0\B';")
        assert ok
        kinds = [t.kind for t in tokens]
        self.assert_token_kinds(
            tokens, [lex.TokenKind.STRING_LITERAL, lex.TokenKind.SEMI]
        )
        self.assertNotIn(lex.TokenKind.BACKSLASH, kinds)
        self.assertEqual(r"'A\X2\00E9\X0\B'", tokens[0].text)

    def test_string_with_quote_escape(self) -> None:
        tokens, ok = lex.lex(r"'O\'Reilly';")
        assert ok
        self.assert_token_kinds(
            tokens, [lex.TokenKind.STRING_LITERAL, lex.TokenKind.SEMI]
        )
        self.assertEqual(r"'O\'Reilly'", tokens[0].text)

    def test_string_simple(self) -> None:
        tokens, ok = lex.lex("'hello';")
        assert ok
        self.assert_token_kinds(
            tokens, [lex.TokenKind.STRING_LITERAL, lex.TokenKind.SEMI]
        )

    def test_empty_block_comment(self) -> None:
        tokens, ok = lex.lex("(* *)")
        assert ok
        self.assert_token_kinds(tokens, [lex.TokenKind.COMMENT])

    def test_garbage_in_block_comment(self) -> None:
        tokens, ok = lex.lex("(* \ntrla baba lan \\ hello \n *)")
        assert ok
        self.assert_token_kinds(tokens, [lex.TokenKind.COMMENT])

    def test_comment_between_tokens(self) -> None:
        tokens, ok = lex.lex("ENTITY(*x*)A;END_ENTITY;")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.ENTITY,
                lex.TokenKind.COMMENT,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.SEMI,
                lex.TokenKind.END_ENTITY,
                lex.TokenKind.SEMI,
            ],
        )

    def test_multiline_comment_between_tokens(self) -> None:
        tokens, ok = lex.lex("ENTITY(* something \n unimportant *)A;END_ENTITY;")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.ENTITY,
                lex.TokenKind.COMMENT,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.SEMI,
                lex.TokenKind.END_ENTITY,
                lex.TokenKind.SEMI,
            ],
        )

    def test_multiple_multiline_comments_between_tokens(self) -> None:
        tokens, ok = lex.lex(
            """\
ENTITY
  (* 
    first comment block 
  *) 
  A 
  (*
    another comment block
  *)
  ;
  END_ENTITY;
"""
        )
        if not ok:
            if len(tokens) == 0:
                raise AssertionError("Failed to parse any tokens")

            tokens_joined = "\n".join(str(token) for token in tokens)
            raise AssertionError(
                "Failed to parse the two blocks; the tokens parsed so far:\n"
                f"{tokens_joined}"
            )

        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.ENTITY,
                lex.TokenKind.COMMENT,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.COMMENT,
                lex.TokenKind.SEMI,
                lex.TokenKind.END_ENTITY,
                lex.TokenKind.SEMI,
            ],
        )

    def test_multiline_prolog_comment_from_IFC4_1_RC3_20170302_schema(self) -> None:
        # NOTE (mristin):
        # This test case failed on the first implementation of the lexer and we only
        # discovered it when we tried to parse the actual schema.

        tokens, ok = lex.lex(
            """\
(*
Copyright by:
buildingSMART International Limited, 1996-2017

Any technical documentation made available by buildingSMART International Limited
is the copyrighted work of buildingSMART International Limited and is owned by the 
buildingSMART International Limited. It may be photocopied, used in software development, 
or translated into another computer language without prior written consent from 
buildingSMART International Limited provided that full attribution is given. 
Prior written consent is required if changes are made to the technical specification.

This material is delivered to you as is and buildingSMART International Limited makes 
no warranty of any kind with regard to it, including, but not limited to, the implied 
warranties as to its accuracy or fitness for a particular purpose. Any use of the 
technical documentation or the information contained therein is at the risk of the user. 
Documentation may include technical or other inaccuracies or typographical errors. 
buildingSMART International Limited shall not be liable for errors contained therein or 
for incidental consequential damages in connection with the furnishing, performance or use 
of the material. The information contained in this document is subject to change without notice.

Issue date:
Thursday, March 2, 2017

*)

SCHEMA IFC4X1;
"""
        )
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.COMMENT,
                lex.TokenKind.SCHEMA,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.SEMI,
            ],
        )

    def test_numbers_with_signs_and_exponents(self) -> None:
        text = (
            "x : INTEGER = -10; y : REAL = 1.23E-3; z : REAL = 0.5; w : INTEGER = +42;"
        )
        tokens, ok = lex.lex(text)
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.COLON,
                lex.TokenKind.INTEGER,
                lex.TokenKind.EQ,
                lex.TokenKind.MINUS,
                lex.TokenKind.INTEGER_LITERAL,
                lex.TokenKind.SEMI,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.COLON,
                lex.TokenKind.REAL,
                lex.TokenKind.EQ,
                lex.TokenKind.REAL_LITERAL,
                lex.TokenKind.SEMI,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.COLON,
                lex.TokenKind.REAL,
                lex.TokenKind.EQ,
                lex.TokenKind.REAL_LITERAL,
                lex.TokenKind.SEMI,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.COLON,
                lex.TokenKind.INTEGER,
                lex.TokenKind.EQ,
                lex.TokenKind.PLUS,
                lex.TokenKind.INTEGER_LITERAL,
                lex.TokenKind.SEMI,
            ],
        )

    def test_real_does_not_require_digits_both_sides(self) -> None:
        tokens, ok = lex.lex(".5 . 1.")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.REAL_LITERAL,
                lex.TokenKind.DOT,
                lex.TokenKind.REAL_LITERAL,
            ],
        )

    def test_list_bounds_and_of(self) -> None:
        tokens, ok = lex.lex("LIST [1:10] OF STRING;")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.LIST,
                lex.TokenKind.LSQ,
                lex.TokenKind.INTEGER_LITERAL,
                lex.TokenKind.COLON,
                lex.TokenKind.INTEGER_LITERAL,
                lex.TokenKind.RSQ,
                lex.TokenKind.OF,
                lex.TokenKind.STRING,
                lex.TokenKind.SEMI,
            ],
        )

    def test_parens_commas_whitespace(self) -> None:
        tokens, ok = lex.lex("crop \t   ( 20, 30, 40, 10 ) ;")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.LPAR,
                lex.TokenKind.INTEGER_LITERAL,
                lex.TokenKind.COMMA,
                lex.TokenKind.INTEGER_LITERAL,
                lex.TokenKind.COMMA,
                lex.TokenKind.INTEGER_LITERAL,
                lex.TokenKind.COMMA,
                lex.TokenKind.INTEGER_LITERAL,
                lex.TokenKind.RPAR,
                lex.TokenKind.SEMI,
            ],
        )

    def test_dot_attribute_without_self(self) -> None:
        tokens, ok = lex.lex("IfcWall.GlobalId;")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.DOT,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.SEMI,
            ],
        )

    def test_control_flow_tokens(self) -> None:
        tokens, ok = lex.lex("IF TRUE THEN x = 1; END_IF;")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.IF,
                lex.TokenKind.TRUE,
                lex.TokenKind.THEN,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.EQ,
                lex.TokenKind.INTEGER_LITERAL,
                lex.TokenKind.SEMI,
                lex.TokenKind.END_IF,
                lex.TokenKind.SEMI,
            ],
        )

    def test_lowercase(self) -> None:
        tokens, ok = lex.lex("if true then x = 1; end_if;")
        assert ok
        self.assert_token_kinds(
            tokens,
            [
                lex.TokenKind.IF,
                lex.TokenKind.TRUE,
                lex.TokenKind.THEN,
                lex.TokenKind.IDENTIFIER,
                lex.TokenKind.EQ,
                lex.TokenKind.INTEGER_LITERAL,
                lex.TokenKind.SEMI,
                lex.TokenKind.END_IF,
                lex.TokenKind.SEMI,
            ],
        )


class TextLexOnSchemas(unittest.TestCase):
    # pylint: disable=protected-access

    def test_success_on_real_schemas(self) -> None:
        for path in sorted((tests.common.TEST_DATA_DIR / "schemas").glob("*.exp")):
            text = path.read_text(encoding="utf-8")

            tokens, ok = lex.lex(text)
            if not ok:
                self.assertGreater(len(tokens), 0, "At least something parsed")

                last_token = tokens[-1]
                end = last_token.start + len(last_token.text)
                missing_text = text[end : end + 50]

                last_token_kinds = [token.kind.value for token in tokens[-10:]]
                self.fail(
                    f"Last tokens were: {last_token_kinds!r}, "
                    f"missing text: {missing_text!r}"
                )


if __name__ == "__main__":
    unittest.main()
