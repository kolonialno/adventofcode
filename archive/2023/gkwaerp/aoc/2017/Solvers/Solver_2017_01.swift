//
//  Solver_2017_01.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 13/11/2023.
//

import Foundation

final class Solver_2017_01: Solver {
    private var input: String = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let result = sumSequence(input, midwayMode: false)
        return "\(result)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let result = sumSequence(input, midwayMode: true)
        return "\(result)"
    }

    func sumSequence(_ string: String, midwayMode: Bool = false) -> Int {
        var sum = 0
        let arrayed = string.convertToStringArray()
        for i in 0..<arrayed.count {
            let offset = midwayMode ? arrayed.count / 2 : 1
            let checkIndex = (i + offset) % arrayed.count
            if arrayed[i] == arrayed[checkIndex] {
                sum += Int(arrayed[i])!
            }
        }
        return sum
    }
}

extension Solver_2017_01: TestableDay {
    func runTests() {
        let inputs1 = ["1122", "1111", "1234", "91212129"]
        let answers1 = [3, 4, 0, 9]
        let results1 = inputs1.map { sumSequence($0, midwayMode: false) }
        assert(answers1 == results1)

        let inputs2 = ["1212", "1221", "123425", "123123", "12131415"]
        let answers2 = [6, 0, 4, 12, 4]
        let results2 = inputs2.map { sumSequence($0, midwayMode: true) }
        assert(answers2 == results2)

    }
}
