//
//  SyntaxScoring.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 10/12/2021.
//

import Foundation

struct SyntaxScoring {

    func part1(_ input: String) -> Int {
        let lines = input.components(separatedBy: "\n").map { $0.map { $0 } }

        var stack: [SyntaxScoring.Bracket] = []

        let scores = lines.map { line -> Int in

            var score: Int = 0
            for char in line {

                guard let bracket = Bracket(rawValue: char) else { fatalError("Unexpected bracket type") }

                if bracket.isOpening {
                    stack.append(bracket)
                } else {
                    if stack.last == bracket.pair {
                        stack.removeLast()
                    } else {
                        score = bracket.corruptionScore
                        stack.removeAll()
                        break
                    }
                }
            }

            return score
        }

        return scores.reduce(0, +)
    }

    func part2(_ input: String) -> Int {
        let lines = input.components(separatedBy: "\n").map { $0.map { $0 } }

        let scores = lines.map { line -> Int? in
            var stack: [SyntaxScoring.Bracket] = []
            var shouldStop = false
            for char in line {

                guard let bracket = Bracket(rawValue: char) else { fatalError("Unexpected bracket type") }

                if bracket.isOpening {
                    stack.append(bracket)
                } else {
                    if stack.last == bracket.pair {
                        stack.removeLast()
                    } else {
                        shouldStop = true
                        break
                    }
                }
            }

            guard !shouldStop else { return nil }

            let missing = stack.reversed().map { $0.pair }

            return missing.reduce(0) { $0 * 5 + $1.autocompletionScore }
        }

        let sorted = scores.compactMap { $0 }.sorted(by: { $0 < $1 })
        
        return sorted[(sorted.count - 1) / 2]
    }
}

extension SyntaxScoring {

    enum Bracket: Character {
        case squareOpeninig = "["
        case squareClosing = "]"

        case curlyOpening = "{"
        case curlyClosing = "}"

        case roundOpening = "("
        case roundClosing = ")"

        case chevronOpening = "<"
        case chevronClosing = ">"

        var isOpening: Bool {
            switch self {
            case .squareOpeninig,
                    .roundOpening,
                    .curlyOpening,
                    .chevronOpening:
                return true
            case .squareClosing,
                    .roundClosing,
                    .curlyClosing,
                    .chevronClosing:
                return false
            }
        }

        var pair: Bracket {
            let pair: Bracket

            switch self {
            case .curlyClosing:
                pair = .curlyOpening
            case .curlyOpening:
                pair = .curlyClosing

            case .roundClosing:
                pair = .roundOpening
            case .roundOpening:
                pair = .roundClosing

            case .squareClosing:
                pair = .squareOpeninig
            case .squareOpeninig:
                pair = .squareClosing

            case .chevronClosing:
                pair = .chevronOpening
            case .chevronOpening:
                pair = .chevronClosing
            }

            return pair
        }

        var corruptionScore: Int {
            let score: Int
            switch self {
            case .roundClosing:
                score = 3
            case .squareClosing:
                score = 57
            case .curlyClosing:
                score = 1197
            case .chevronClosing:
                score = 25137
            case .chevronOpening,
                    .roundOpening,
                    .curlyOpening,
                    .squareOpeninig:
                assertionFailure("why accessing corruption score of an opening bracket?")
                score = 0
            }

            return score
        }

        var autocompletionScore: Int {
            let score: Int

            switch self {
            case .roundClosing:
                score = 1
            case .squareClosing:
                score = 2
            case .curlyClosing:
                score = 3
            case .chevronClosing:
                score = 4
            case .curlyOpening,
                    .roundOpening,
                    .squareOpeninig,
                    .chevronOpening:
                assertionFailure("why accessing autocompletion score of an opening bracket?")
                score = 0
            }

            return score
        }
    }
}
