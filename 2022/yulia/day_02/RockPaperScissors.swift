//
//  RockPaperScissors.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 02/12/2022.
//

import Foundation

struct RockPaperScissors {

    func part1(_ input: String) -> Int {

        enum Shape: String {
            case a = "A"
            case b = "B"
            case c = "C"
            case x = "X"
            case y = "Y"
            case z = "Z"

            var score: Int {
                let result: Int
                switch self {
                case .a, .x: result = 1
                case .b, .y: result = 2
                case .c, .z: result = 3
                }

                return result
            }
        }

        let rounds = input.components(separatedBy: "\n")

        return rounds.map { roundString -> Int in
            let components = roundString.components(separatedBy: " ")
            let elf = Shape(rawValue: components[0])!
            let me = Shape(rawValue: components[1])!

            let outcomeScore: Int
            switch (elf, me) {
            case (.a, .y), (.b, .z), (.c, .x):
                outcomeScore = 6
            case (.a, .z), (.b, .x), (.c, .y):
                outcomeScore = 0
            default:
                outcomeScore = 3
            }

            return outcomeScore + me.score
        }
        .reduce(0, +)
    }

    func part2(_ input: String) -> Int {

        enum Shape: String {
            case a = "A"
            case b = "B"
            case c = "C"

            var score: Int {
                let result: Int
                switch self {
                case .a: result = 1
                case .b: result = 2
                case .c: result = 3
                }

                return result
            }
        }

        enum Outcome: String {
            case x = "X"
            case y = "Y"
            case z = "Z"

            var score: Int {
                let result: Int
                switch self {
                case .x: result = 0
                case .y: result = 3
                case .z: result = 6
                }

                return result
            }
        }

        let rounds = input.components(separatedBy: "\n")

        return rounds.map { roundString -> Int in
            let components = roundString.components(separatedBy: " ")
            let elf = Shape(rawValue: components[0])!
            let outcome = Outcome(rawValue: components[1])!

            let shape: Shape

            switch (elf, outcome) {
            case (.a, .x), (.c, .y), (.b, .z): shape = .c
            case (.a, .y), (.b, .x), (.c, .z) : shape = .a
            case (.a, .z), (.b, .y), (.c, .x): shape = .b
            }

            return outcome.score + shape.score
        }
        .reduce(0, +)
    }

    func part1Alternative(_ input: String) -> Int {
        let rounds = input.components(separatedBy: "\n")

        return rounds.map { round -> Int in
            let values = round.compactMap { $0.asciiValue }

            let elf = values[0] - 64 // a:1 b:2 c:3
            let me = values[2] - 87 // x:1 y:2 z:3

            let outcome: Int
            switch (elf, me) {
            case (1, 2), (2, 3), (3, 1):
                outcome = 6
            case (1, 3), (2, 1), (3, 2):
                outcome = 0
            default:
                outcome = 3
            }

            return outcome + Int(me)
        }
        .reduce(0, +)
    }

    func part2Alternative(_ input: String) -> Int {
        let rounds = input.components(separatedBy: "\n")

        return rounds.map { round -> Int in
            let values = round.compactMap { $0.asciiValue }

            let elf = values[0] - 64
            let outcome = (values[2] - 88) * 3

            let score: Int
            switch (elf, outcome) {
            case (1, 0), (2, 6), (3, 3):
                score = 3
            case (1, 3), (2, 0), (3, 6):
                score = 1
            default:
                score = 2
            }

            return Int(outcome) + score
        }
        .reduce(0, +)
    }
}
