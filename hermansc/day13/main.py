lines = open("schedule.txt").read().strip()

# Part 1
earliest, departures = (int(lines.split("\n")[0]), [int(bid) for bid in lines.split("\n")[1].split(",") if bid != "x"])
nd = min([(bid - (earliest % bid), bid) for bid in departures])
print(nd[1] * nd[0])

# Part 2 (First naive attempt - never finishes)
# departures = [int(bid) if bid != "x" else None for bid in lines.split("\n")[1].split(",")]
# i = departures[0]
# while True:
#     for idx, v in enumerate(departures[1:]):
#         if not v:
#             continue
#         if v - (i % v) != (idx + 1):
#             break
#     else:
#         print(i)
#         break
#     i += departures[0]

# Part 2
departures = [(int(bid), idx) for idx, bid in enumerate(lines.split("\n")[1].split(",")) if bid != "x"]
i = departures[0][0]
pos = 0
for t, offset in departures[1:]:
    while (pos + offset) % t != 0:
        pos += i
    i *= t
print(pos)
