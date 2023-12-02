limit = {
    "red":"12",
    "green":"13",
    "blue":"14",
}
result = 0
powersum = 0
for line in out.splitlines():
    gameid, line = line.split(":")
    gameid = gameid.split(" ")[1]
    possible = True
    redmax, bluemax, greenmax = [0,0,0]
    for round in line.split(";"):
        for cube in round.split(","):
            splitted = cube.split(" ")
            splitted.remove("")
            digit, color = splitted
            if color == "red":
                redmax = max(redmax, int(digit))
            if color == "blue":
                bluemax = max(bluemax, int(digit))
            if color == "green":
                greenmax = max(greenmax, int(digit))
            if int(limit[color]) < int(digit):
                # print("not possible")
                # print(line)
                possible = False
    if possible:
        result += int(gameid)
        
    print(redmax,bluemax,greenmax)
    power = redmax*bluemax*greenmax
    print(power)
    powersum += power
print(result)
print(powersum)
