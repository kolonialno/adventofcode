import pytest

@pytest.fixture
def data_getter():
    def get_data(day):
        with open(f"tests/{day}/input.txt") as f:
            return f.read()
    return get_data
