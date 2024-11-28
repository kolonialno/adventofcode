//
//  Solver_2017_02.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 13/11/2023.
//

import Foundation

final class Solver_2017_02: Solver {
    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let checksum = calculateChecksum(input)
        return "\(checksum)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let checksum = calculateChecksum2(input)
        return "\(checksum)"
    }

    private func calculateChecksum(_ strings: [String]) -> Int {
        var sum = 0
        strings.forEach { row in
            let arrayedInts = row
                .components(separatedBy: .whitespaces)
                .filter { !$0.isEmpty }
                .map { Int($0)! }
            let minimum = arrayedInts.min()!
            let maximum = arrayedInts.max()!
            sum += maximum - minimum
        }
        return sum
    }

    private func calculateChecksum2(_ strings: [String]) -> Int {
        var sum = 0
        strings.forEach { row in
            let arrayedInts = row
                .components(separatedBy: .whitespaces)
                .filter { !$0.isEmpty }
                .map { Int($0)! }
            var found = false
            for i in 0..<arrayedInts.count - 1 {
                guard !found else { break }
                for j in (i + 1)..<arrayedInts.count {
                    let a = arrayedInts[i]
                    let b = arrayedInts[j]
                    let minimum = min(a, b)
                    let maximum = max(a, b)
                    if maximum % minimum == 0 {
                        sum += maximum / minimum
                        found = true
                        break
                    }
                }
            }
        }
        return sum
    }
}

extension Solver_2017_02: TestableDay {
    func runTests() {
        let testInput = """
5 1 9 5
7 5 3
2 4 6 8
"""
            .components(separatedBy: .newlines)
        let checksum = calculateChecksum(testInput)
        assert(checksum == 18)

        let testInput2 = """
5 9 2 8
9 4 7 3
3 8 6 5
"""
            .components(separatedBy: .newlines)
        let checksum2 = calculateChecksum2(testInput2)
        assert(checksum2 == 9)
    }
}
