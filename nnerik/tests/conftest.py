import pytest


@pytest.fixture
def data_getter():
    def get_data(day, suffix=""):
        with open(f"tests/test_samples/sample{day:02}{suffix}.txt") as f:
            return f.read()

    return get_data
