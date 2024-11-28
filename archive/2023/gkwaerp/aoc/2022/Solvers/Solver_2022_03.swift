//
//  Solver_2022_03.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 03/12/2022.
//

import Foundation

class Solver_2022_03: Solver {
    class Rucksack {
        private let compartments: [Set<String>]
        let fullInventory: Set<String>

        init(_ string: String) {
            let charArray = string.convertToStringArray()
            let mid = charArray.count / 2
            let part1 = charArray[..<mid]
            let part2 = charArray[mid...]

            self.compartments = [Set(part1), Set(part2)]
            self.fullInventory = Set(charArray)
        }

        func getOverlapping() -> String {
            let intersection = compartments[0].intersection(compartments[1])
            assert(intersection.count == 1)
            return intersection.first!
        }
    }

    private var rucksacks: [Rucksack] = []

    override func didLoadFunction() {
        rucksacks = defaultInputFileString
            .loadAsStringArray()
            .map { Rucksack($0) }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let result = getPrioritySum(for: rucksacks)
        return "\(result)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let divided = divideInGroups(rucksacks: rucksacks)
        let badges = divided.map { getBadge(for: $0) }
        let result = badges.map { $0.priority }.reduce(0, +)
        return "\(result)"
    }

    private func getPrioritySum(for rucksacks: [Rucksack]) -> Int {
        rucksacks.map { $0.getOverlapping().priority }.reduce(0, +)
    }

    private func getBadge(for rucksacks: [Rucksack]) -> String {
        assert(rucksacks.count == 3)
        let remainingRucksacks = Array(rucksacks.dropFirst())
        let intersection = remainingRucksacks.reduce(rucksacks[0].fullInventory, { $0.intersection($1.fullInventory) })
        assert(intersection.count == 1)
        return intersection.first!
    }

    private func divideInGroups(rucksacks: [Rucksack]) -> [[Rucksack]] {
        assert(rucksacks.count % 3 == 0)
        let groups = stride(from: 0, to: rucksacks.count, by: 3)
            .map {
                Array(rucksacks[$0...$0 + 2])
            }
        return groups
    }
}

extension Solver_2022_03: TestableDay {
    func runTests() {
        let testInput = """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""
            .components(separatedBy: .newlines)

        assert("a".priority == 1)
        assert("z".priority == 26)
        assert("A".priority == 27)
        assert("Z".priority == 52)

        let rucksacks = testInput.map { Rucksack($0) }
        let prioritySum = getPrioritySum(for: rucksacks)
        assert(prioritySum == 157)

        let divided = divideInGroups(rucksacks: rucksacks)
        assert(divided.count == 2)
        let badges = divided.map { getBadge(for: $0) }
        assert(badges[0] == "r")
        assert(badges[1] == "Z")
    }
}

private extension String {
    var priority: Int {
        assert(count == 1)
        if lowercased() == self {
            let offset = "a".first!.asciiValue!
            return Int(first!.asciiValue! - offset + 1)
        } else {
            let offset = "A".first!.asciiValue!
            return Int(first!.asciiValue! - offset + 27)
        }
    }
}
