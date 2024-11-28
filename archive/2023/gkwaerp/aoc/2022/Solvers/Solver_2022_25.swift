//
//  Solver_2022_25.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 25/12/2022.
//

import Foundation

class Solver_2022_25: Solver {
    class Bob {
        /// SNAFU numbers
        let numbers: [String]

        init(numbers: [String]) {
            self.numbers = numbers
        }

        func getSNAFUNumber() -> String {
            let decimalSum = numbers
                .map { $0.snafuToDecimal() }
                .reduce(0, +)
            let decimalString = "\(decimalSum)"

            let inputCode = decimalString.decimalToSnafu()
            return inputCode
        }
    }
    override func solveFunction1() -> CustomStringConvertible {
        let input = defaultInputFileString.loadAsStringArray()
        let bob = Bob(numbers: input)
        let inputCode = bob.getSNAFUNumber()
        return "\(inputCode)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        return "Merry Christmas!"
    }
}

extension Solver_2022_25: TestableDay {
    func runTests() {
        let tests1: [(String, String)] = [("1", "1"), ("2", "2"), ("3", "1="), ("4", "1-"), ("5", "10"), ("6", "11"), ("7", "12"), ("8", "2="), ("9", "2-"), ("10", "20"),
                                       ("15", "1=0"), ("20", "1-0"), ("2022", "1=11-2"), ("12345", "1-0---0"), ("314159265", "1121-1110-1=0")]

        tests1.forEach { (decimal, snafu) in
            assert("\(snafu.snafuToDecimal())" == decimal)
            assert(decimal.decimalToSnafu() == snafu)
        }
    }
}

fileprivate extension String {
    func decimalToSnafu() -> String {
        var values: [String] = []
        var number = Int(self)!
        while number > 0 {
            let mod = number % 5
            if mod <= 2 {
                values.append("\(mod)")
            } else {
                number += mod
                values.append(mod == 3 ? "=" : "-")
            }
            number /= 5
        }
        return values.reversed().joined()
    }

    func snafuToDecimal() -> Int {
        var power = 1
        var sum = 0
        self.enumerated().reversed().forEach { (index, char) in
            let val: Int
            if char == "-" {
                val = -1
            } else if char == "=" {
                val = -2
            } else {
                val = Int("\(char)")!
            }
            sum += val * power
            power *= 5
        }
        return sum
    }
}
