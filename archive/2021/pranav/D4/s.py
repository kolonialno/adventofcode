class Bingo:
    def __init__(self, board: list[str]):
        self.board = [int(x) for x in board]
        self.sheet = [1] * 5 * 5
        self.has_finished = False
        self.last_drawn = None

    def __str__(self):
        return f"{self.board}\n{self.sheet}"

    def mark(self, drawn_number):
        try:
            index = self.board.index(drawn_number)
            self.sheet[index] = 0
        except ValueError:
            pass

    def has_won(self):
        x = 0
        while x != 25:
            if sum(self.sheet[x:x + 5]) == 0:
                return True
            x += 5        
        
        x = 0
        while x != 5:
            if sum(self.sheet[x::5]) == 0:
                return True
            x += 1        
        return False

    def score(self, last_drawn):
        return last_drawn * sum(x*y for x, y in zip(self.board, self.sheet))


def func1():
    with open("input.txt") as f:
        lines = f.readlines()

    host = [int(x) for x in lines.pop(0).split(',')]
    lines.pop(0)

    sheets = [lines[x:x+5] for x in range(0, len(lines), 6)]
    bingos = [Bingo(" ".join(sheet).split()) for sheet in sheets]

    
    for h in host:
        for b in bingos:
            b.mark(h)
            if b.has_won():
                return b.score(h)

def func2():
    with open("input.txt") as f:
        lines = f.readlines()

    host = [int(x) for x in lines.pop(0).split(',')]
    lines.pop(0)

    sheets = [lines[x:x+5] for x in range(0, len(lines), 6)]
    bingos = [Bingo(" ".join(sheet).split()) for sheet in sheets]

    winners: list[Bingo]=[]
    for h in host:
        for b in bingos:
            if b.has_finished: continue

            b.mark(h)
            if b.has_won():
                b.has_finished = True
                b.last_drawn = h
                winners.append(b)
                
    winner = winners.pop()
    return winner.score(winner.last_drawn)

        
print(func1())
print(func2())