//
//  Solver_2017_09.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 16/11/2023.
//

import Foundation

final class Solver_2017_09: Solver {
    private struct GarbageInfo {
        let cleanString: String
        let removedCount: Int
    }

    private var input = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let garbageFree = removeGarbage(input).cleanString
        let score = getScore(garbageFree)
        return score
    }

    override func solveFunction2() -> CustomStringConvertible {
        let result = removeGarbage(input).removedCount
        return result
    }

    private func removeGarbage(_ string: String) -> GarbageInfo {
        var shouldSkipNext = false
        var isInsideGarbage = false

        var removedCount = 0
        var result = ""

        string.forEach { char in
            if shouldSkipNext {
                shouldSkipNext = false
                return
            }

            switch char {
            case "!":
                shouldSkipNext = true
            case "<":
                if isInsideGarbage {
                    removedCount += 1
                } else {
                    isInsideGarbage = true
                }
            case ">":
                if !isInsideGarbage {
                    removedCount += 1
                }
                isInsideGarbage = false
            default:
                guard !isInsideGarbage else {
                    removedCount += 1
                    return
                }
                result.append(char)
            }

        }

        return GarbageInfo(cleanString: result, removedCount: removedCount)
    }

    private func getScore(_ string: String) -> Int {
        var score = 0
        var groupNesting = 0

        string.forEach { char in
            switch char {
            case "{":
                groupNesting += 1
                score += groupNesting
            case "}":
                groupNesting -= 1
            default:
                break
            }
        }

        return score
    }
}

extension Solver_2017_09: TestableDay {
    func runTests() {
        let inputs = [
            "{}",
            "{{{}}}",
            "{{},{}}",
            "{{{},{},{{}}}}",
            "{<a>,<a>,<a>,<a>}",
            "{{<ab>},{<ab>},{<ab>},{<ab>}}",
            "{{<!!>},{<!!>},{<!!>},{<!!>}}",
            "{{<a!>},{<a!>},{<a!>},{<ab>}}"
        ]

        let garbageFree = inputs.map { removeGarbage($0).cleanString }
        let results = garbageFree.map { getScore($0) }
        let expected = [1, 6, 5, 16, 1, 9, 9, 3]

        assert(results == expected)

        let inputs2 = [
            "<>",
            "<random characters>",
            "<<<<>",
            "<{!>}>",
            "<!!>",
            "<!!!>>",
            #"<{o"i!a,<{i<a>"#
        ]

        let garbageFree2 = inputs2.map { removeGarbage($0).removedCount }
        let expected2 = [0, 17, 3, 2, 0, 0, 10]

        assert(garbageFree2 == expected2)
    }
}
