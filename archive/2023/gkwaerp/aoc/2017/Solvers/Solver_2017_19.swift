//
//  Solver_2017_19.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2023.
//

import Foundation

final class Solver_2017_19: Solver {
    final class MazeRunner {
        struct Result {
            let path: String
            let steps: Int
        }
        let grid: StringGrid

        init(grid: StringGrid) {
            self.grid = grid
        }

        func getPath() -> Result {
            var path = ""
            var steps = 0
            var currPos = findStartPos()
            var currDirection = Direction.south

            while true {
                currPos += currDirection.movementVector
                steps += 1
                guard let value = grid.getValue(at: currPos), !value.isEmpty, value != " " else { break }
                switch value {
                case "+":
                    let leftTurn = currDirection.turn(left: true)
                    let rightTurn = currDirection.turn(left: false)
                    let leftPos = currPos + leftTurn.movementVector

                    if let leftValue = grid.getValue(at: leftPos), !leftValue.isEmpty, leftValue != " " {
                        currDirection = leftTurn
                    } else {
                        currDirection = rightTurn
                    }
                case "-", "|":
                    break
                default:
                    path.append(value)
                }
            }

            

            return Result(path: path, steps: steps)
        }

        private func findStartPos() -> IntPoint {
            for x in 0..<grid.size.x {
                guard let value = grid.getValue(at: IntPoint(x: x, y: 0)) else { continue }
                if !value.isEmpty && value != " " {
                    return IntPoint(x: x, y: 0)
                }
            }

            fatalError("StartPos not found")
        }
    }

    private var grid: StringGrid!

    override func didLoadFunction() {
        grid = defaultInputFileString.loadAsStringGrid(trimming: false)
    }

    override func solveFunction1() -> CustomStringConvertible {
        let mazeRunner = MazeRunner(grid: grid)
        return mazeRunner.getPath().path
    }

    override func solveFunction2() -> CustomStringConvertible {
        let mazeRunner = MazeRunner(grid: grid)
        return mazeRunner.getPath().steps
    }
}


extension Solver_2017_19: TestableDay {
    func runTests() {
        let input = ["     |          ",
                     "     |  +--+    ",
                     "     A  |  C    ",
                     " F---|----E|--+ ",
                     "     |  |  |  D ",
                     "     +B-+  +--+ ",
                     "                "]
        let stringGrid = StringGrid(stringArray: input)
        let mazeRunner = MazeRunner(grid: stringGrid)
        let result = mazeRunner.getPath()
        let expectedPath = "ABCDEF"
        let expectedSteps = 38
        assert(result.path == expectedPath)
        assert(result.steps == expectedSteps)
    }
}
