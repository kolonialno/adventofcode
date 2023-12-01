//
//  Solver_2022_02.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 02/12/2022.
//

import Foundation

class Solver_2022_02: Solver {
    enum Outcome: Int {
        case lose = 0
        case draw = 1
        case win = 2

        var score: Int {
            rawValue * 3
        }

        init(_ string: String) {
            switch string {
            case "Z":
                self = .win
            case "Y":
                self = .draw
            case "X":
                self = .lose
            default:
                fatalError("Unsupported string \(string)")
            }
        }

        static func getSign(for outcome: Outcome, opponent: Sign) -> Sign {
            switch outcome {
            case .draw:
                return opponent

            case .win:
                let newRawValue = (opponent.rawValue + 1) % 3
                return Sign(rawValue: newRawValue)!

            case .lose:
                let newRawValue = (opponent.rawValue + 2) % 3
                return Sign(rawValue: newRawValue)!
            }
        }
    }

    enum Sign: Int {
        case rock = 0
        case paper = 1
        case scissors = 2

        var score: Int {
            rawValue + 1
        }

        init(_ string: String) {
            switch string {
            case "A", "X":
                self = .rock
            case "B", "Y":
                self = .paper
            case "C", "Z":
                self = .scissors
            default:
                fatalError("Unsupported string \(string)")
            }
        }
    }

    class Round {
        private let opponent: Sign
        private let you: Sign

        init(_ string: String, parseSecondAsOutcome: Bool) {
            let components = string.components(separatedBy: " ")
            opponent = .init(components[0])

            if parseSecondAsOutcome {
                let wantedOutcome = Outcome(components[1])
                you = Outcome.getSign(for: wantedOutcome, opponent: opponent)
            } else {
                you = .init(components[1])
            }
        }

        func getYourScore() -> Int {
            getYourOutcome().score + you.score
        }

        private func getYourOutcome() -> Outcome {
            if opponent == you {
                return .draw
            }

            let delta = (opponent.rawValue - you.rawValue + 3) % 3
            return delta == 2 ? .win : .lose
        }
    }

    class MatchResolver {
        private let rounds: [Round]

        init(_ strings: [String], parseSecondAsOutcome: Bool) {
            rounds = strings.map { Round($0, parseSecondAsOutcome: parseSecondAsOutcome) }
        }

        func getYourTotalScore() -> Int {
            rounds
                .map { $0.getYourScore() }
                .reduce(0, +)
        }
    }

    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let matchResolver = MatchResolver(input, parseSecondAsOutcome: false)
        let result = matchResolver.getYourTotalScore()
        return "\(result)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let matchResolver = MatchResolver(input, parseSecondAsOutcome: true)
        let result = matchResolver.getYourTotalScore()
        return "\(result)"
    }
}

extension Solver_2022_02: TestableDay {
    func runTests() {
        let testInput = """
A Y
B X
C Z
"""
            .components(separatedBy: .newlines)

        let matchResolver1 = MatchResolver(testInput, parseSecondAsOutcome: false)
        let totalScore1 = matchResolver1.getYourTotalScore()
        assert(totalScore1 == 15)

        let matchResolver2 = MatchResolver(testInput, parseSecondAsOutcome: true)
        let totalScore2 = matchResolver2.getYourTotalScore()
        assert(totalScore2 == 12)
    }
}
