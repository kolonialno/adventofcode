//
//  Solver_2022_01.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class Solver_2022_01: Solver {
    class Elf {
        let calories: Int

        private init(food: String) {
            calories = food
                .components(separatedBy: "\n")
                .map { Int($0)! }
                .reduce(0, +)
        }

        static func createSorted(from input: [String]) -> [Elf] {
            input
                .map { Elf(food: $0) }
                .sorted(by: { $0.calories > $1.calories })
        }

        static func getCalories(from elves: [Elf], numToInclude num: Int) -> Int {
            elves
                .prefix(num)
                .map { $0.calories }
                .reduce(0, +)
        }
    }

    private var elves: [Elf] = []

    override func didLoadFunction() {
        let input = defaultInputFileString.loadAsTextStringArray(separator: "\n\n")
        elves = Elf.createSorted(from: input)
    }

    override func solveFunction1() -> String {
        let result = Elf.getCalories(from: elves, numToInclude: 1)
        return "\(result)"
    }

    override func solveFunction2() -> String {
        let result = Elf.getCalories(from: elves, numToInclude: 3)
        return "\(result)"
    }
}

extension Solver_2022_01: TestableDay {
    func runTests() {
        let testInput = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""
            .components(separatedBy: "\n\n")

        let elves = Elf.createSorted(from: testInput)
        let top1 = Elf.getCalories(from: elves, numToInclude: 1)
        assert(top1 == 24_000)

        let top3 = Elf.getCalories(from: elves, numToInclude: 3)
        assert(top3 == 45_000)
    }
}
