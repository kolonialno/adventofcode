//
//  Solver_2023_01.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 01/12/2023.
//

import Foundation

final class Solver_2023_01: Solver {
    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        getCalibrationValue(input, allowText: false)
    }

    override func solveFunction2() -> CustomStringConvertible {
        getCalibrationValue(input, allowText: true)
    }

    private func getCalibrationValue(_ input: [String], allowText: Bool) -> Int {
        let strings = allowText ? convertStrings(input) : input

        let calibrations = strings.map { string in
            let arrayed = string.convertToStringArray().compactMap { $0.intValue }
            let value = Int("\(arrayed.first!)\(arrayed.last!)")!
            return value
        }

        return calibrations.reduce(0, +)
    }

    private func convertStrings(_ strings: [String]) -> [String] {
        let conversions = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]
        let mapped = strings.map { string in
            var newString = ""
            let arrayed = string.convertToStringArray()
            for i in 0..<arrayed.count {
                if arrayed[i].intValue != nil {
                    newString += arrayed[i]
                } else {
                    let joined = arrayed[i...].joined()
                    conversions.forEach { s, i in
                        if joined.hasPrefix(s) {
                            newString += "\(i)"
                        }
                    }
                }
            }

            return newString
        }

        return mapped
    }
}

extension Solver_2023_01: TestableDay {
    func runTests() {
        let input = """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""
            .components(separatedBy: .newlines)

        let result1 = getCalibrationValue(input, allowText: false)
        let expected1 = 142
        assert(result1 == expected1)

        let input2 = """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""
            .components(separatedBy: .newlines)

        let result2 = getCalibrationValue(input2, allowText: true)
        let expected2 = 281
        assert(result2 == expected2)
    }
}
