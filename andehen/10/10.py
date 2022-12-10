def is_signal_strength_cycle(cycle):
    return cycle == 20 or (cycle - 20) % 40 == 0


def get_pixel_pos(cycle):
    return cycle // 40, cycle % 40


def draw_screen(screen, cycle, x):
    pixel_row, pixel_column = get_pixel_pos(cycle)
    if abs(pixel_column - x) < 2:
        screen[pixel_row][pixel_column] = "#"


def perform_cycle():
    global cycle, x, screen, signal_strengths
    draw_screen(screen, cycle, x)
    cycle += 1
    if is_signal_strength_cycle(cycle):
        signal_strengths.append(cycle * int(x))


instructions = []
with open("input.txt") as f:
    for l in f.readlines():
        instructions.append(l.strip().split(" "))


cycle = 0
x = 1
signal_strengths = []
screen = [["."] * 40 for _ in range(6)]


for i in instructions:
    match i:
        case ["noop"]:
            perform_cycle()

        case ["addx", value]:
            perform_cycle()
            perform_cycle()
            x += int(value)

print(sum(signal_strengths))
for s in screen:
    print("".join(s))
