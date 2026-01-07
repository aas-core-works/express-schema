"""Provide features shared accross the tests."""

import os
import pathlib

RECORDING_VAR = "EXPRESS_SCHEMA_TEST_RECORD"
RECORDING = os.environ.get(RECORDING_VAR, "").lower() in (
    "on",
    "1",
    "true",
)

TEST_DATA_DIR = pathlib.Path(os.path.realpath(__file__)).parent.parent / "test_data"
