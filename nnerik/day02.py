# Read input
input = open(0).read().strip().split("\n")
reports = [[int(level) for level in report.split()] for report in input]


# Part 1
def is_safe(report):
    sign = -1 if report[0] > report[1] else 1
    for i in range(len(report) - 1):
        if not 1 <= sign * (report[i + 1] - report[i]) <= 3:
            return False
    return True


print("Part 1:", sum(1 for report in reports if is_safe(report)))


# Part 2
def is_safe_2(report):
    for skip in range(len(report)):
        if is_safe(report[:skip] + report[skip + 1 :]):
            return True
    return False


print("Part 2:", sum(1 for report in reports if is_safe_2(report)))
