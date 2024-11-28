//
//  Solver_2023_06.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 06/12/2023.
//

import Foundation

final class Solver_2023_06: Solver {
    struct Race {
        let time: Int
        let bestDistance: Int

        func getNumWaysToBeatRecord() -> Int {
            (1..<time).reduce(0) { $0 + (doesWinIfWaiting(for: $1) ? 1 : 0) }
        }

        private func doesWinIfWaiting(for waitTime: Int) -> Bool {
            let speed = waitTime
            let remainingTime = time - waitTime
            return speed * remainingTime > bestDistance
        }
    }

    struct RaceManager {
        let races: [Race]

        init(_ strings: [String], customKerning: Bool) {
            if customKerning {
                let ints = strings
                    .map { string in
                        string
                            .components(separatedBy: .whitespaces)
                            .filter { $0.intValue != nil }
                            .joined()
                            .intValue!
                    }
                races = [Race(time: ints[0], bestDistance: ints[1])]
            } else {
                let ints = strings
                    .map { string in
                        string
                            .components(separatedBy: .whitespaces)
                            .compactMap { $0.intValue }
                    }
                let zipped = zip(ints[0], ints[1])
                races = zipped.map { Race(time: $0.0, bestDistance: $0.1) }
            }
        }

        func getWinningWaysFactor() -> Int {
            races.reduce(1, { $0 * $1.getNumWaysToBeatRecord() })
        }
    }

    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        RaceManager(input, customKerning: false).getWinningWaysFactor()
    }

    override func solveFunction2() -> CustomStringConvertible {
        RaceManager(input, customKerning: true).getWinningWaysFactor()
    }
}

extension Solver_2023_06: TestableDay {
    func runTests() {
        let input = """
Time:      7  15   30
Distance:  9  40  200
"""
            .components(separatedBy: .newlines)

        let result1 = RaceManager(input, customKerning: false).getWinningWaysFactor()
        let expected1 = 288
        assert(result1 == expected1)

        let result2 = RaceManager(input, customKerning: true).getWinningWaysFactor()
        let expected2 = 71503
        assert(result2 == expected2)
    }
}
