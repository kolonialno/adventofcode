//
//  Solver_2017_04.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 14/11/2023.
//

import Foundation

final class Solver_2017_04: Solver {

    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let validCount = input.map { isValid($0) }
            .filter { $0 }
            .count

        return "\(validCount)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let validCount = input.map { isValidStrict($0) }
            .filter { $0 }
            .count

        return "\(validCount)"
    }

    private func isValid(_ passphrase: String) -> Bool {
        let words = passphrase.components(separatedBy: .whitespaces)
        let wordSet = Set(words)
        return words.count == wordSet.count
    }

    private func isValidStrict(_ passphrase: String) -> Bool {
        let words = passphrase.components(separatedBy: .whitespaces).map { String($0.sorted()) }
        let wordSet = Set(words)
        return words.count == wordSet.count
    }
}


extension Solver_2017_04: TestableDay {
    func runTests() {
        let inputs = [
            "aa bb cc dd ee",
            "aa bb cc dd aa",
            "aa bb cc dd aaa"
        ]
        let answers = inputs.map { isValid($0) }
        let expected = [true, false, true]
        assert(answers == expected)

        let inputs2 = [
            "abcde fghij",
            "abcde xyz ecdab",
            "a ab abc abd abf abj",
            "iiii oiii ooii oooi oooo",
            "oiii ioii iioi iiio"
        ]
        let answers2 = inputs2.map { isValidStrict($0) }
        let expected2 = [true, false, true, true, false]
        assert(answers2 == expected2)
    }
}
