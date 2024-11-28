//
//  Solver_2023_04.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 04/12/2023.
//

import Foundation

final class Solver_2023_04: Solver {
    struct Scratchcard {
        let id: Int
        let winningNumbers: Set<Int>
        let activeNumbers: [Int]

        init(_ string: String) {
            let split = string.components(separatedBy: ": ")
            self.id = split[0]
                .replacingOccurrences(of: "Card", with: "")
                .trimmingCharacters(in: .whitespaces)
                .intValue!

            let numberSplit = split[1]
                .components(separatedBy: " | ")

            self.winningNumbers = Set(numberSplit[0]
                .components(separatedBy: " ")
                .map { $0.trimmingCharacters(in: .whitespaces) }
                .filter { !$0.isEmpty }
                .map { $0.intValue! })

            self.activeNumbers = numberSplit[1]
                .components(separatedBy: " ")
                .map { $0.trimmingCharacters(in: .whitespaces) }
                .filter { !$0.isEmpty }
                .map { $0.intValue! }
        }

        var pointValue: Int {
            winningNumbers
                .intersection(activeNumbers)
                .reduce(1, { prev, _ in prev * 2 }) / 2
        }

        var numMatching: Int {
            winningNumbers.intersection(activeNumbers).count
        }
    }

    private var scratchcards: [Scratchcard] = []

    override func didLoadFunction() {
        scratchcards = defaultInputFileString.loadAsStringArray().map { Scratchcard($0) }
    }

    override func solveFunction1() -> CustomStringConvertible {
        getPointValueSum(scratchcards)
    }

    override func solveFunction2() -> CustomStringConvertible {
        getTotalScratchcardCount(scratchcards)
    }


    private func getPointValueSum(_ cards: [Scratchcard]) -> Int {
        cards
            .map { $0.pointValue }
            .reduce(0, +)
    }

    private func getTotalScratchcardCount(_ scratchcards: [Scratchcard]) -> Int {
        let uniqueCount = scratchcards.count
        var dict: [Int: Int] = [:]
        scratchcards.forEach { dict[$0.id] = 1 }

        scratchcards.forEach { scratchcard in
            let numSelf = dict[scratchcard.id]!
            let numMatching = scratchcard.numMatching
            for i in 0..<numMatching {
                let wonId = scratchcard.id + i + 1
                guard wonId <= uniqueCount else { break }
                dict[wonId]! += numSelf
            }
        }

        return dict.values.reduce(0, +)
    }
}

extension Solver_2023_04: TestableDay {
    func runTests() {
        let scratchcards = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""
            .components(separatedBy: .newlines)
            .map { Scratchcard($0) }

        let result1 = getPointValueSum(scratchcards)
        let expected1 = 13
        assert(result1 == expected1)

        let result2 = getTotalScratchcardCount(scratchcards)
        let expected2 = 30
        assert(result2 == expected2)
    }
}
