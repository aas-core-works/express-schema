**************
express-schema
**************

.. image:: https://github.com/aas-core-works/express-schema/actions/workflows/ci.yml/badge.svg
    :target: https://github.com/aas-core-works/express-schema/actions/workflows/ci.yml
    :alt: Continuous integration

.. image:: https://coveralls.io/repos/github/aas-core-works/express-schema/badge.svg?branch=main
    :target: https://coveralls.io/github/aas-core-works/express-schema

.. image:: https://badge.fury.io/py/express-schema.svg
    :target: https://badge.fury.io/py/express-schema
    :alt: PyPI - version

.. image:: https://img.shields.io/pypi/pyversions/express-schema.svg
    :alt: PyPI - Python Version

Parse EXPRESS Schema (ISO 10303-11).

Installation
============
Create a virtual environment:

.. code-block::

    python -m venv venv

Activate it (on Windows):

.. code-block::

    venv/Scripts/activate

... or on Unix:

.. code-block::

    source venv/bin/activate

Install the dependencies:

.. code-block::

    pip3 install express-schema

You can now use the modules ``express_schema.parse`` (see "Usage" section below) or run ``express-schema`` (see ``--help`` section below).

Usage
=====
The Abstract Syntax Tree and parsing functionality lives in ``express_schema.parse`` module.

Example:

.. code-block::

    >>> import express_schema.parse as parse
    >>>
    >>> # Define a simple EXPRESS schema as a string
    >>> schema_text = '''SCHEMA SimpleExample;
    ...
    ... ENTITY Person;
    ...   name : STRING;
    ...   age : INTEGER;
    ... END_ENTITY;
    ...
    ... END_SCHEMA;'''
    >>>
    >>> # Parse the schema
    >>> schema, errors = parse.parse(schema_text)
    >>>
    >>> # Check for parsing errors
    >>> if errors is not None:
    ...     print("Parsing errors occurred:")
    ...     for error in errors:
    ...         print(
    ...             f"  Line {error.line}, Column {error.column}: "
    ...             f"{error.message}"
    ...         )
    ... else:
    ...     print("Schema parsed successfully!")
    ...     print(f"Schema name: {schema.identifier}")
    ...     print(f"Number of entities: {len(schema.entity_declarations)}")
    ...
    ...     # Dump the parsed schema back to text
    ...     dumped_text = parse.dump(schema)
    ...     print("Dumped schema:")
    ...     print(dumped_text)
    Schema parsed successfully!
    Schema name: SimpleExample
    Number of entities: 1
    Dumped schema:
    Schema(
      position=1:1,
      identifier='SimpleExample',
      reference_clauses=[],
      use_clauses=[],
      constant_definitions=[],
      entity_declarations=[EntityDeclaration(
          position=3:1,
          identifier='Person',
          explicit_clauses=[
              ExplicitAttributeDefinition(
                position=4:3,
                attributes=[NameRef(
                    position=4:3,
                    identifier='name'
                  )],
                optional=False,
                type_selection=StringType(
                    position=4:10,
                    fixed=False
                  )
              ),
              ExplicitAttributeDefinition(
                position=5:3,
                attributes=[NameRef(
                    position=5:3,
                    identifier='age'
                  )],
                optional=False,
                type_selection=IntegerType(
                    position=5:9
                  )
              )
            ],
          derive_clauses=[],
          inverse_clauses=[],
          unique_rules=[],
          domain_rules=[]
        )],
      function_declarations=[],
      procedure_declarations=[],
      type_declarations=[],
      rule_declarations=[]
    )

If, for some reason, you only want to tokenize and not parse the schema, you can use ``express_schema.lex`` module.

Example:

.. code-block::

    >>> import express_schema.lex as lex
    >>>
    >>> # Define a simple EXPRESS schema text to tokenize
    >>> schema_text = '''SCHEMA Example;
    ... ENTITY Person;
    ...   name : STRING;
    ... END_ENTITY;
    ... END_SCHEMA;'''
    >>>
    >>> # Tokenize the schema
    >>> tokens, success = lex.lex(schema_text)
    >>>
    >>> # Check if tokenization was successful
    >>> if success:
    ...     print("Tokenization successful!")
    ...     print(f"Number of tokens: {len(tokens)}")
    ...     print("First few tokens:")
    ...     for token in tokens[:5]:
    ...         print(
    ...             f"  {token.kind.name} at {token.position}: "
    ...             f"{token.text!r}"
    ...         )
    ... else:
    ...     print("Tokenization failed!")
    Tokenization successful!
    Number of tokens: 14
    First few tokens:
      SCHEMA at 1:1: 'SCHEMA'
      IDENTIFIER at 1:8: 'Example'
      SEMI at 1:15: ';'
      ENTITY at 2:1: 'ENTITY'
      IDENTIFIER at 2:8: 'Person'

``--help``
==========

.. Help starts: express-schema --help
.. code-block::

    usage: express-schema [-h] --schema SCHEMA [--version]

    Parse EXPRESS Schema (ISO 10303-11) and print the Abstract Syntax Tree.

    options:
      -h, --help       show this help message and exit
      --schema SCHEMA  Path to the EXPRESS schema file
      --version        show the current version and exit

.. Help ends: express-schema --help

Contributing
============
See `CONTRIBUTING.rst`_.

.. _CONTRIBUTING.rst: CONTRIBUTING.rst

Change Log
==========
1.0.2 (2026-01-07)
------------------
A very misleading reference to Git checkout is removed from the readme (the package is available on PyPI, so no checkout is necessary).

1.0.1 (2026-01-07)
------------------
We add the ``py.typed`` marker so that downstream clients can use this package with mypy.

1.0.0 (2026-01-07)
------------------
This is the first version where we successfully performed the experiments with IFC to AAS conversion.
