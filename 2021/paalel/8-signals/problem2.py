
class bidict(dict):
    def __init__(self, *args, **kwargs):
        super(bidict, self).__init__(*args, **kwargs)

    def rev(self, value):
        for k, v in self.items():
            if v == value:
                return set(k)

    def get(self, key):
        for k, v in self.items():
            if set(k) == set(key):
                return str(v)

        return ""


def solve(filename):
    return_value = 0

    with open(filename) as file:
        for line in file.readlines():
            sm = bidict()
            (i, o) = line.strip().split("|")
            i = i.lstrip().split(" ")
            i.sort(key=lambda x: len(x))

            if i[0] == "":
                i.pop(0)

            # 1,4,7,8
            sm[i.pop(-1)] = 8
            sm[i.pop(2)] = 4
            sm[i.pop(1)] = 7
            sm[i.pop(0)] = 1

            def add_to_mapping(i,n,length,j,l):
                for s in i:
                    if len(s) == length and len((sm.rev(8) - set(s)) & sm.rev(l)) == j:
                        sm[s] = n
                        i.remove(s)
                        return

            # n=3, length=5 --- 8 - 3 should intersect 0(j) with 1(l)
            add_to_mapping(i,n=3,length=5,j=0,l=1)

            # n=6, length=6 --- 8 - 6 should intersect 1(j) with 1(l)
            add_to_mapping(i,n=6,length=6,j=1,l=1)

            # n=9, length=6 --- 8 - 9 should intersect 0(j) with 4(l)
            add_to_mapping(i,n=9,length=6,j=0,l=4)

            # n=0 -- only with 6 symbols left
            sm[i.pop(-1)] = 0

            # n=2, length=5 --- 8 - 2 should intersect 2(j) with 9(l)
            add_to_mapping(i,n=2,length=5,j=2,l=9)

            # n=5 -- only symbol left
            sm[i.pop(0)] = 5

            return_value += int("".join([sm.get(s) for s in o.split(" ")]))

    return return_value



assert solve("test_input.txt") == 61229
print("solution to problem 2: ", solve("input.txt"))
