from functools import reduce
from operator import mul
from pathlib import Path
from textwrap import wrap
from typing import Tuple


def parse_literal(bits: str) -> Tuple[int, str]:
    groups = wrap(bits, 5)
    literal = ""
    for i, group in enumerate(groups):
        literal += group[1:]
        if group[0] == "0":
            rest = "".join(groups[i + 1:])
            literal = int(literal, base=2)
            return literal, rest
    raise RuntimeError

def parse(bits):
    version_bits, type_id_bits, bits = bits[:3], bits[3:6], bits[6:]
    version = int(version_bits, base=2)
    type_id = int(type_id_bits, base=2)
    result = {"version": version, "type_id": type_id}

    if type_id == 4:
        literal_value, bits = parse_literal(bits)
        result["content"] = literal_value
        result["remaining"] = bits
        return result
    else:
        length_type_id, bits = bits[0], bits[1:]
        if length_type_id == "0":
            length_of_bits = int(bits[:15], base=2)
            bits = bits[15:]
            subpackets = bits[:length_of_bits]
            parsed_subpacket = parse(subpackets)
            parsed_subpackets = [parsed_subpacket]
            while parsed_subpacket["remaining"]:
                parsed_subpacket = parse(parsed_subpacket["remaining"])
                parsed_subpackets.append(parsed_subpacket)
            result["content"] = parsed_subpackets
            result["remaining"] = bits[length_of_bits:]
            return result
        else:
            number_of_subpackets = int(bits[:11], base=2)
            bits = bits[11:]
            parsed_subpacket = parse(bits)
            parsed_subpackets = [parsed_subpacket]
            while len(parsed_subpackets) != number_of_subpackets:
                parsed_subpacket = parse(parsed_subpacket["remaining"])
                parsed_subpackets.append(parsed_subpacket)
            result["content"] = parsed_subpackets
            result["remaining"] = parsed_subpackets[-1]["remaining"]
            return result


def version_sum(content):
    if content["type_id"] == 4:
        return content["version"]
    else:
        return content["version"] + sum(
            version_sum(subcontent)
            for subcontent
            in content["content"]
        )


def evaluate(content) -> int:
    type_id = content["type_id"]
    if type_id == 0:
        return sum(evaluate(subcontent) for subcontent in content["content"])
    if type_id == 1:
        return reduce(mul, (evaluate(subcontent) for subcontent in content["content"]))
    if type_id == 2:
        return min(evaluate(subcontent) for subcontent in content["content"])
    if type_id == 3:
        return max(evaluate(subcontent) for subcontent in content["content"])
    if type_id == 4:
        return content["content"]
    if type_id == 5:
        return int(evaluate(content["content"][0]) > evaluate(content["content"][1]))
    if type_id == 6:
        return int(evaluate(content["content"][0]) < evaluate(content["content"][1]))
    if type_id == 7:
        return int(evaluate(content["content"][0]) == evaluate(content["content"][1]))
    raise RuntimeError


puzzle = Path("inputs/16.txt").read_text().strip()
bits = "".join(str(bin(int(char, base=16))[2:]).zfill(4) for char in puzzle)
content = parse(bits)
print(version_sum(content))
print(evaluate(content))
