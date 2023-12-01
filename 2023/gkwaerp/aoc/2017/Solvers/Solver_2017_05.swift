//
//  Solver_2017_05.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 14/11/2023.
//

import Foundation

final class Solver_2017_05: Solver {
    private var input: [Int] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsIntArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let result = getJumpsBeforeExit(list: input, strangeRules: false)
        return "\(result)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let result = getJumpsBeforeExit(list: input, strangeRules: true)
        return "\(result)"
    }

    private func getJumpsBeforeExit(list constList: [Int], strangeRules: Bool) -> Int {
        let listSize = constList.count

        var list = constList
        var index = 0
        var numSteps = 0
        while true {
            let offset = list[index]

            // Jump
            let oldIndex = index
            index += offset
            numSteps += 1
            
            // Increase jumped instruction
            if strangeRules {
                list[oldIndex] += (offset >= 3) ? -1 : 1
            } else {
                list[oldIndex] += 1
            }

            if !(0..<listSize).contains(index) {
                break
            }
        }

        return numSteps
    }
}

extension Solver_2017_05: TestableDay {
    func runTests() {
        let input = """
0
3
0
1
-3
"""
            .components(separatedBy: .newlines)
            .map { Int($0)! }

        let result = getJumpsBeforeExit(list: input, strangeRules: false)
        let expected = 5
        assert(result == expected)

        let result2 = getJumpsBeforeExit(list: input, strangeRules: true)
        let expected2 = 10
        assert(result2 == expected2)
    }
}
