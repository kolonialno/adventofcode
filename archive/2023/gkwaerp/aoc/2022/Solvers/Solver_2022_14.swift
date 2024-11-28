//
//  Solver_2022_14.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 14/12/2022.
//

import Foundation

class Solver_2022_14: Solver {
    class Cave {
        private var caveMap: [IntPoint: String] = [:]
        private let sandSpawnPoint = IntPoint(x: 500, y: 0)
        private let floorHeight: Int
        private let useFloor: Bool
        private let solver: Solver?

        private(set) var numSandAtRest = 0

        init(string: String, rememberFloor: Bool, solver: Solver? = nil) {
            self.solver = solver
            var rockPoints: Set<IntPoint> = []
            let rockPaths = string.components(separatedBy: .newlines)
            rockPaths.forEach { rockPath in
                let points = rockPath
                    .components(separatedBy: " -> ")
                    .map { pointString in
                            pointString
                            .components(separatedBy: ",")
                            .map { Int($0)! }
                    }.map { IntPoint(x: $0[0], y: $0[1]) }


                rockPoints.insert(points.first!)
                points[1...].enumerated().forEach { (index, point) in
                    IntPoint.line(from: points[index], to: point).forEach { rockPoints.insert($0) }
                }
            }

            floorHeight = rockPoints.map { $0.y }.max()! + 2
            useFloor = rememberFloor

            rockPoints.forEach { caveMap[$0] = "#" }
            caveMap[sandSpawnPoint] = "+"
        }

        func simulate() {
            var shouldSpawnSand = true
            while shouldSpawnSand {
                var sandPos = sandSpawnPoint

                while let nextPosition = getNextSandPosition(currentPosition: sandPos) {
                    sandPos = nextPosition
                }

                if isSandInBounds(at: sandPos) {
                    numSandAtRest += 1
                    solver?.visualizeCurrentPart(text: "\(numSandAtRest)")

                    caveMap[sandPos] = "o"
                    if sandPos == sandSpawnPoint {
                        shouldSpawnSand = false
                    }
                } else {
                    shouldSpawnSand = false
                }
            }
        }

        private func getNextSandPosition(currentPosition: IntPoint) -> IntPoint? {
            let offsets: [IntPoint] = [IntPoint(x: 0, y: 1),
                                       IntPoint(x: -1, y: 1),
                                       IntPoint(x: 1, y: 1)]

            for i in 0..<offsets.count {
                let offset = offsets[i]
                let candidatePosition = currentPosition + offset
                if candidatePosition.y >= floorHeight {
                    return nil
                }

                if caveMap[candidatePosition] == nil {
                    return candidatePosition
                }
            }

            return nil
        }

        private func isSandInBounds(at position: IntPoint) -> Bool {
            position.y <= floorHeight + (useFloor ? 0 : -2)
        }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let input = defaultInputFileString.loadAsTextString()
        let cave = Cave(string: input, rememberFloor: false, solver: self)
        cave.simulate()
        return "\(cave.numSandAtRest)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let input = defaultInputFileString.loadAsTextString()
        let cave = Cave(string: input, rememberFloor: true, solver: self)
        cave.simulate()
        return "\(cave.numSandAtRest)"
    }
}
