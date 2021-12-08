//
//  SevenSegmentSearch.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 08/12/2021.
//

import Foundation
import UIKit

struct SevenSegmentSearch {

    func part1(_ input: String) -> Int {
        let outputValues = Parser().parseInput(input)

        return uniqueCount(outputValues)
    }

    private func uniqueCount(_ outputValues: [String]) -> Int {
        let uniqueNumbersOfSegments = Set<Int>([2, 4, 3, 7])
        let values = outputValues.filter { value -> Bool in
            uniqueNumbersOfSegments.contains(value.count)
        }

        return values.count
    }

    func part2(_ input: String) -> Int {
        let lines = input.components(separatedBy: "\n")

        return lines.map { number(for: $0) }.reduce(0, +)
    }

    private func number(for line: String) -> Int {
        let signals = line.components(separatedBy: "|")[0].components(separatedBy: " ").dropLast()

        let digits = signals
            .map { $0.map { SevenSegmentSearch.Digit.fromChar($0) } }
            .map { SevenSegmentSearch.Digit($0) }

        var fives: [SevenSegmentSearch.Digit] = []
        var sixes: [SevenSegmentSearch.Digit] = []

        var one: SevenSegmentSearch.Digit?
        var four: SevenSegmentSearch.Digit?
        var seven: SevenSegmentSearch.Digit?
        var eight: SevenSegmentSearch.Digit?

        for digit in digits {
            if digit.rawValue.nonzeroBitCount == 2 {
                // this is ONE
                one = digit
            } else if digit.rawValue.nonzeroBitCount == 3 {
                // this is SEVEN
                seven = digit
            } else if digit.rawValue.nonzeroBitCount == 4 {
                four = digit
            } else if digit.rawValue.nonzeroBitCount == 5 {
                // this is TWO or THREE or FIVE
                fives.append(digit)
            } else if digit.rawValue.nonzeroBitCount == 6 {
                // this is ZERO or SIX or NINE
                sixes.append(digit)
            } else {
                eight = digit

                // this is EIGHT
            }
        }

        var zero: SevenSegmentSearch.Digit?
        var two: SevenSegmentSearch.Digit?
        var three: SevenSegmentSearch.Digit?
        var five: SevenSegmentSearch.Digit?
        var six: SevenSegmentSearch.Digit?
        var nine: SevenSegmentSearch.Digit?

        for element in fives {
            let intersectWith4 = element.intersection(four!)
            let intersectWith7 = element.intersection(seven!)

            if intersectWith7.rawValue.nonzeroBitCount == 3 {
                three = element
            } else if intersectWith4.rawValue.nonzeroBitCount == 3 {
                five = element
            } else {
                two = element
            }
        }

        for element in sixes {
            let intersectWith1 = element.intersection(one!)
            let intersectWith4 = element.intersection(four!)

            if intersectWith1.rawValue.nonzeroBitCount == 1 {
                six = element
            } else if intersectWith4.rawValue.nonzeroBitCount == 4 {
                nine = element
            } else {
                zero = element
            }
        }

        let outputValues = line.components(separatedBy: "|")[1].components(separatedBy: " ")
            .dropFirst()
            .map { $0.map { SevenSegmentSearch.Digit.fromChar($0) } }
            .map { SevenSegmentSearch.Digit($0) }

        let numbers = outputValues.map {

            let number: Int
            switch $0 {
            case one:
                number = 1
            case two:
                number = 2
            case three:
                number = 3
            case four:
                number = 4
            case five:
                number = 5
            case six:
                number = 6
            case seven:
                number = 7
            case eight:
                number = 8
            case nine:
                number = 9
            case zero:
                number = 0
            default:
                fatalError("panic")
            }

            return number
        }
            .reduce(0, { $0 * 10 + $1 })

        return numbers
    }
}

extension SevenSegmentSearch {

    struct Parser {

        func parseInput(_ input: String) -> [String] {
            let lines = input.components(separatedBy: "\n")
            let outputValues = lines.map { $0.components(separatedBy: "|")[1] }.map { $0.components(separatedBy: " ") }

            return outputValues.flatMap { $0  }
        }
    }

    struct Digit: OptionSet {
        let rawValue: Int

        static let a: Digit = Digit(rawValue: 1 << 0)
        static let b: Digit = Digit(rawValue: 1 << 1)
        static let c: Digit = Digit(rawValue: 1 << 2)
        static let d: Digit = Digit(rawValue: 1 << 3)
        static let e: Digit = Digit(rawValue: 1 << 4)
        static let f: Digit = Digit(rawValue: 1 << 5)
        static let g: Digit = Digit(rawValue: 1 << 6)

        static func fromChar(_ char: Character) -> Digit {
            switch char {
            case "a":
                return .a
            case "b":
                return .b
            case "c":
                return .c
            case "d":
                return .d
            case "e":
                return .e
            case "f":
                return .f
            case "g":
                return .g
            default:
                fatalError("wat")
            }
        }
    }
}

