//
//  Solver_2017_11.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 24/11/2023.
//

import Foundation

final class Solver_2017_11: Solver {
    private struct Result {
        let finalDistance: Int
        let maxDistance: Int
    }

    private var input = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString()
    }

    override func solveFunction1() -> CustomStringConvertible {
        return getDistance(after: input).finalDistance
    }

    override func solveFunction2() -> CustomStringConvertible {
        return getDistance(after: input).maxDistance
    }

    private func getDistance(after path: String) -> Result {
        func absMax(a: Int, b: Int, c: Int) -> Int {
            max(abs(a), max(abs(b), abs(c)))
        }

        var q = 0
        var r = 0
        var s = 0

        var maxDistance = 0

        let directions = path.components(separatedBy: ",")

        directions.forEach { direction in
            switch direction {
            case "n":
                r -= 1
                s += 1
            case "nw":
                q -= 1
                s += 1
            case "ne":
                q += 1
                r -= 1
            case "se":
                q += 1
                s -= 1
            case "s":
                r += 1
                s -= 1
            case "sw":
                q -= 1
                r += 1
            default: break
            }

            let currentDistance = absMax(a: q, b: r, c: s)
            maxDistance = max(maxDistance, currentDistance)
        }

        let finalDistance = absMax(a: q, b: r, c: s)
        return Result(finalDistance: finalDistance, maxDistance: maxDistance)
    }
}

extension Solver_2017_11: TestableDay {
    func runTests() {
        let input = ["ne,ne,ne", "ne,ne,sw,sw", "ne,ne,s,s", "se,sw,se,sw,sw"]
        let results = input.map { getDistance(after: $0).finalDistance }
        let expected = [3, 0, 2, 3]
        assert(results == expected)
    }
}
