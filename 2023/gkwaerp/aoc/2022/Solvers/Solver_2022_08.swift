//
//  Solver_2022_08.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 08/12/2022.
//

import Foundation

class Solver_2022_08: Solver {
    private var grid: IntGrid!

    override func didLoadFunction() {
        grid = defaultInputFileString.loadAsIntGrid()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let numVisible = countVisibleTrees(in: grid)
        return "\(numVisible)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let bestScenicScore = getBestScenicScore(for: grid)
        return "\(bestScenicScore)"
    }

    private func countVisibleTrees(in grid: IntGrid) -> Int {
        var visiblePoints: Set<IntPoint> = []

        func updateVisiblePoints(pos: IntPoint, tallest: Int) -> Int {
            let currentHeight = grid.getValue(at: pos)!

            if currentHeight > tallest {
                visiblePoints.insert(pos)
                return currentHeight
            }

            return tallest
        }

        // East <--> West
        (1..<(grid.height - 1)).forEach { y in
            // From west, looking east
            var tallest = grid.getValue(at: IntPoint(x: 0, y: y))!
            (1..<(grid.width - 1)).forEach { tallest = updateVisiblePoints(pos: IntPoint(x: $0, y: y), tallest: tallest) }

            // From east, looking west
            tallest = grid.getValue(at: IntPoint(x: grid.width - 1, y: y))!
            (1..<(grid.width - 1)).reversed().forEach { tallest = updateVisiblePoints(pos: IntPoint(x: $0, y: y), tallest: tallest) }
        }

        // North <--> South
        (1..<(grid.width - 1)).forEach { x in
            // From north, looking south
            var tallest = grid.getValue(at: IntPoint(x: x, y: 0))!
            (1..<(grid.height - 1)).forEach { tallest = updateVisiblePoints(pos: IntPoint(x: x, y: $0), tallest: tallest) }

            // From south, looking north
            tallest = grid.getValue(at: IntPoint(x: x, y: grid.height - 1))!
            (1..<(grid.height - 1)).reversed().forEach { tallest = updateVisiblePoints(pos: IntPoint(x: x, y: $0), tallest: tallest) }
        }

        // Border trees
        (0..<grid.width).forEach { x in
            visiblePoints.insert(IntPoint(x: x, y: 0))
            visiblePoints.insert(IntPoint(x: x, y: grid.height - 1))
        }

        (0..<grid.height).forEach { y in
            visiblePoints.insert(IntPoint(x: 0, y: y))
            visiblePoints.insert(IntPoint(x: grid.width - 1, y: y))
        }

        return visiblePoints.count
    }

    private func getScenicScore(for pos: IntPoint, in grid: IntGrid) -> Int {
        var numVisible: [Direction: Int] = [:]

        let ownHeight = grid.getValue(at: pos)!
        for direction in Direction.allCases {
            var nextPos = pos + direction.movementVector

            while let nextHeight = grid.getValue(at: nextPos) {
                numVisible[direction, default: 0] += 1

                guard nextHeight < ownHeight else {
                    break
                }

                nextPos += direction.movementVector
            }
        }

        return Direction.allCases
            .map { numVisible[$0, default: 0] }
            .reduce(1, *)
    }

    private func getBestScenicScore(for grid: IntGrid) -> Int {
        grid.gridPoints
            .map { getScenicScore(for: $0, in: grid) }
            .max()!
    }
}

extension Solver_2022_08: TestableDay {
    func runTests() {
        let testInput = """
30373
25512
65332
33549
35390
"""
            .components(separatedBy: .newlines)
            .filter { !$0.isEmpty }
        let grid = IntGrid(stringArray: testInput)

        let numVisible = countVisibleTrees(in: grid)
        assert(numVisible == 21)

        assert(getScenicScore(for: IntPoint(x: 2, y: 1), in: grid) == 4)
        assert(getScenicScore(for: IntPoint(x: 2, y: 3), in: grid) == 8)

        let bestScenicScore = getBestScenicScore(for: grid)
        assert(bestScenicScore == 8)
    }
}
