//
//  Solver_2017_10.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 16/11/2023.
//

import Foundation

final class Solver_2017_10: Solver {

    private var input = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let intInput = input.components(separatedBy: ",").map { Int($0)! }
        let hasher = Common_2017.KnotHasher(listSize: 256, inputLengths: intInput)
        hasher.runHash()
        return hasher.getHashValue()
    }

    override func solveFunction2() -> CustomStringConvertible {
        let hasher = Common_2017.KnotHasher(listSize: 256, string: input)
        hasher.runHash(numRounds: 64)
        return hasher.getHexString()
    }
}

extension Solver_2017_10: TestableDay {
    func runTests() {
        let inputLengths = [3, 4, 1, 5]
        let hasher = Common_2017.KnotHasher(listSize: 5, inputLengths: inputLengths)
        hasher.runHash()
        let hashValue = hasher.getHashValue()
        let expected = 12
        assert(hashValue == expected)

        let inputs2 = ["", "AoC 2017", "1,2,3", "1,2,4"]
        let results2 = inputs2.map { input in
            let hasher = Common_2017.KnotHasher(listSize: 256, string: input)
            hasher.runHash(numRounds: 64)
            return hasher.getHexString()
        }

        let expected2 = [
            "a2582a3a0e66e6e86e3812dcb672a272",
            "33efeb34ea91902bb2f59c9920caa6cd",
            "3efbe78a8d82f29979031a4aa0b16a9d",
            "63960835bcdc130f0b66d7ff4f6a5a8e"
        ]
        assert(results2 == expected2)
    }
}
