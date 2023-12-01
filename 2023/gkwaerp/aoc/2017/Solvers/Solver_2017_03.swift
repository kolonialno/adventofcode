//
//  Solver_2017_03.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 13/11/2023.
//

import Foundation

final class Solver_2017_03: Solver {

    private var input = -1
    private var pointMap: [Int: IntPoint] = [:]
    private var valueMap: [IntPoint: Int] = [:]
    private var solution2: Int? = nil

    override func didLoadFunction() {
        input = Int(defaultInputFileString.loadAsTextString())!

        var pos = IntPoint(x: 0, y: 0)
        var direction = Direction.east
        var numMovesBeforeTurn = 1
        var currentMaxBeforeTurn = 1
        var numTurnsWithoutIncrease = 0
        pointMap[1] = pos
        valueMap[pos] = 1
        for i in 2...input {
            pos = pos.move(in: direction, numSteps: 1)
            pointMap[i] = pos

            if solution2 == nil {
                let neighborSum: Int = IntPoint.allDirectionOffsets
                    .map { pos + $0 } // Neighbor position
                    .map { valueMap[$0] } // Neighbor value, if written
                    .compactMap { $0 } // Filtered
                    .reduce(0, +)
                valueMap[pos] = neighborSum
                if neighborSum > input {
                    solution2 = neighborSum
                }
            }

            numMovesBeforeTurn -= 1
            if numMovesBeforeTurn <= 0 {
                direction.turn(left: true)
                numTurnsWithoutIncrease += 1
                if numTurnsWithoutIncrease == 2 {
                    numTurnsWithoutIncrease = 0
                    currentMaxBeforeTurn += 1
                }
                numMovesBeforeTurn = currentMaxBeforeTurn
            }
        }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let result = calculateSteps(for: input)
        return "\(result)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        return "\(solution2!)"
    }

    private func calculateSteps(for index: Int) -> Int {
        let endPos = pointMap[index]!
        return endPos.manhattanDistance()
    }
}

extension Solver_2017_03: TestableDay {
    func runTests() {
        let inputs = [1, 12, 23, 1024]
        let results = inputs.map { calculateSteps(for: $0) }
        let answers = [0, 3, 2, 31]
        assert(results == answers)
    }
}
