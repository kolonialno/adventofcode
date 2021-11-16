from collections import Counter


def read_program(filename):
    with open(filename) as in_file:
        program_lines = (x.split(" ") for x in in_file.readlines())
    return [(x.strip(), int(y)) for x, y in program_lines]


class Interpreter:
    def __init__(self, program):
        self.accumulator = 0
        self.execution_pointer = 0
        self.program_counter = Counter()
        self.program = program

    def nop(self, argument):
        pass

    def acc(self, argument):
        self.accumulator = self.accumulator + argument

    def jmp(self, argument):
        self.execution_pointer = self.execution_pointer + argument - 1

    def __call__(self):
        while self.execution_pointer < len(self.program):
            op, arg = self.program[self.execution_pointer]

            self.program_counter.update((self.execution_pointer,))
            if self.program_counter[self.execution_pointer] > 1:
                raise ValueError("Infinite loop detected")

            self.execution_pointer += 1
            getattr(self, op)(arg)


def accumulator_dump(filename) -> int:
    try:
        simple_program = Interpreter(read_program(filename))
        simple_program()
    except ValueError as e:
        print(e)
    return simple_program.accumulator


def testcase() -> int:
    return accumulator_dump("inputs/08testcase.txt")


def main() -> int:
    return accumulator_dump("inputs/08.txt")


def secondary() -> int:
    input_code = read_program("inputs/08.txt")
    for line_no in range(0, len(input_code)):
        if input_code[line_no][0] in ("jmp", "nop"):
            new_code = input_code.copy()
            op, arg = new_code[line_no]
            if op == "jmp":
                op = "nop"
            else:
                op = "jmp"
            new_code[line_no] = op, arg
            try:
                mod_prog = Interpreter(new_code)
                mod_prog()
                return mod_prog.accumulator
            except ValueError:
                pass
