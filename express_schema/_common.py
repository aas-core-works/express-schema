"""Provide functionality shared across modules."""

import enum
import textwrap
from typing import cast, NoReturn, TypeVar, Collection, TypeGuard, Iterable

from icontract import require, ensure


@ensure(lambda text, result: text.startswith("\n") or not result.startswith("\n"))
def indent_but_first_line(text: str, indent: str = "  ") -> str:
    """
    Indent all but the first line.

    Examples:
    >>> indent_but_first_line("Line 1\\nLine 2\\nLine 3")
    'Line 1\\n  Line 2\\n  Line 3'

    >>> indent_but_first_line("Single line only")
    'Single line only'

    >>> indent_but_first_line("First\\nSecond", indent=">> ")
    'First\\n>> Second'

    >>> indent_but_first_line("")
    ''

    >>> indent_but_first_line("Just one line\\n")
    'Just one line\\n'
    """
    split = text.split("\n", 1)
    if len(split) == 1:
        return text
    first, rest = split
    return first + "\n" + textwrap.indent(rest, indent)


def bullet_points(points: Iterable[str]) -> str:
    """Make a bullet point list out of the given points."""
    indent = "  "
    return "\n".join(f"* {indent_but_first_line(point, indent)}" for point in points)


class NonNegativeInt(int):
    """Represent a non-negative integer, *i.e.*, ``>= 0``."""

    @require(lambda value: value >= 0)
    def __new__(cls, value: int) -> "NonNegativeInt":
        return cast(NonNegativeInt, value)


def assert_never(value: NoReturn) -> NoReturn:
    """
    Signal to mypy to perform an exhaustive matching.

    Please see the following page for more details:
    https://hakibenita.com/python-mypy-exhaustive-checking
    """
    assert False, f"Unhandled value: {value} ({type(value).__name__})"


EnumT = TypeVar("EnumT", bound=enum.Enum)
EnumLiteralT = TypeVar("EnumLiteralT", bound=enum.Enum)


def is_literal_in(
    literal: EnumT, allowed: Collection[EnumLiteralT]
) -> TypeGuard[EnumLiteralT]:
    """
    Check that the ``literal`` is in a set of ``allowed`` literals.

    An assertion with this function is necessary to make the mypy infer
    the correct types.

    Usually, you write something like:

    ..code-block::

        class Kind(enum.Enum):
            Something = enum.auto()
            Another = enum.auto()
            YetAnother = enum.auto()

        SomeLiteral = Literal[
            Kind.Something,
            Kind.Another
        ]

        SOME_LITERAL_SET: FrozenSet[SomeLiteral] = frozenset(get_args(SomeLiteral))

    Now you can infer:

    ..code-block::

       kind: Kind = Kind.Something  # This is general type.

       # Here is how we can narrow it:
       assert is_literal_in(kind, SOME_LITERAL_SET)

       reveal_type(kind)
       # Revealed type is
       #    Literal[play_with_is_literal_in.Kind.Something]
       #    | Literal[play_with_is_literal_in.Kind.Another]
    """
    return literal in allowed
