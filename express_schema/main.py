"""Parse EXPRESS Schema (ISO 10303-11) and print the Abstract Syntax Tree."""

import argparse
import pathlib
import sys

import express_schema
import express_schema.parse

from express_schema._common import bullet_points


def main(prog: str) -> int:
    """
    Execute the main routine.

    :param prog: name of the program to be displayed in the help
    :return: exit code
    """
    parser = argparse.ArgumentParser(prog=prog, description=__doc__)
    parser.add_argument(
        "--schema",
        help="Path to the EXPRESS schema file",
        required=True,
    )
    parser.add_argument(
        "--version", help="show the current version and exit", action="store_true"
    )

    # NOTE (mristin):
    # The module ``argparse`` is not flexible enough to understand special options such
    # as ``--version`` so we manually hard-wire.
    if "--version" in sys.argv and "--help" not in sys.argv:
        print(express_schema.__version__)
        return 0

    args = parser.parse_args()

    schema_pth = pathlib.Path(args.schema)

    text = schema_pth.read_text(encoding="utf-8")

    schema, errors = express_schema.parse.parse(text)

    if errors is not None:
        error_strs = [str(error) for error in errors]
        print(
            f"There were one or more errors while parsing --schema {schema_pth}:\n"
            f"{bullet_points(error_strs)}",
            file=sys.stderr,
        )
        return 1

    assert schema is not None

    print(express_schema.parse.dump(schema))

    return 0


def entry_point() -> int:
    """Provide an entry point for a console script."""
    return main(prog="express-schema")


if __name__ == "__main__":
    sys.exit(main(prog="express-schema"))
