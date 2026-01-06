"""Run express-schema as Python module."""

import express_schema.main

if __name__ == "__main__":
    # The ``prog`` needs to be set in the argparse.
    # Otherwise, the program name in the help shown to the user will be ``__main__``.
    express_schema.main.main(prog="express_schema")
