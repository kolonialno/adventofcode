//
//  Solver_2022_04.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 04/12/2022.
//

import Foundation

class Solver_2022_04: Solver {
    class SectionAssignment {
        private let elf1: ClosedRange<Int>
        private let elf2: ClosedRange<Int>

        init(_ string: String) {
            let splits = string.components(separatedBy: ",")
                .map { $0.components(separatedBy: "-")
                        .map { Int($0)! }
                }

            self.elf1 = splits[0][0]...splits[0][1]
            self.elf2 = splits[1][0]...splits[1][1]
        }

        var fullyOverlaps: Bool {
            (elf1.contains(elf2.lowerBound) && elf1.contains(elf2.upperBound)) ||
            (elf2.contains(elf1.lowerBound) && elf2.contains(elf1.upperBound))
        }

        var anyOverlap: Bool {
            elf1.overlaps(elf2)
        }
    }

    private var sectionAssignments: [SectionAssignment] = []

    override func didLoadFunction() {
        sectionAssignments = defaultInputFileString.loadAsTextStringArray()
            .map { .init($0) }
    }

    override func solveFunction1() -> String {
        let result = countOverlaps(in: sectionAssignments, includePartial: false)
        return "\(result)"
    }

    override func solveFunction2() -> String {
        let result = countOverlaps(in: sectionAssignments, includePartial: true)
        return "\(result)"
    }

    private func countOverlaps(in assignments: [SectionAssignment], includePartial: Bool) -> Int {
        assignments
            .filter { includePartial ? $0.anyOverlap : $0.fullyOverlaps }
            .count
    }

}

extension Solver_2022_04: TestableDay {
    func runTests() {
        let testInput = """
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"""
            .components(separatedBy: .newlines)

        let assignments = testInput
            .map { SectionAssignment($0) }

        let fullOverlaps = countOverlaps(in: assignments, includePartial: false)
        assert(fullOverlaps == 2)

        let anyOverlaps = countOverlaps(in: assignments, includePartial: true)
        assert(anyOverlaps == 4)
    }
}
