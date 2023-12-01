//
//  Solver_2022_18.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 18/12/2022.
//

import Foundation

class Solver_2022_18: Solver {
    class IntPoint3: Hashable, Equatable {
        let x: Int
        let y: Int
        let z: Int

        static func ==(lhs: IntPoint3, rhs: IntPoint3) -> Bool {
            lhs.x == rhs.x &&
            lhs.y == rhs.y &&
            lhs.z == rhs.z
        }

        static func +(lhs: IntPoint3, rhs: IntPoint3) -> IntPoint3 {
            IntPoint3(x: lhs.x + rhs.x,
                      y: lhs.y + rhs.y,
                      z: lhs.z + rhs.z)
        }

        func hash(into hasher: inout Hasher) {
            hasher.combine(x)
            hasher.combine(y)
            hasher.combine(z)
        }

        init(x: Int, y: Int, z: Int) {
            self.x = x
            self.y = y
            self.z = z
        }

        init(string: String) {
            let coordinates = string.components(separatedBy: ",").map { Int($0)! }
            assert(coordinates.count == 3)
            self.x = coordinates[0]
            self.y = coordinates[1]
            self.z = coordinates[2]
        }
    }

    private var cubes: Set<IntPoint3> = []

    override func didLoadFunction() {
        cubes = Set(defaultInputFileString.loadAsStringArray().map { IntPoint3(string: $0) })
    }

    private let neighborOffsets: Set<IntPoint3> = [
        IntPoint3(x: 1, y: 0, z: 0), IntPoint3(x: -1, y: 0, z: 0),
        IntPoint3(x: 0, y: 1, z: 0), IntPoint3(x: 0, y: -1, z: 0),
        IntPoint3(x: 0, y: 0, z: 1), IntPoint3(x: 0, y: 0, z: -1)
    ]

    private func getSurfaceArea(of cubes: Set<IntPoint3>) -> Int {
        cubes.map { cube in
            neighborOffsets
                .map({ cubes.contains(cube + $0) ? 1 : 0 })
                .reduce(6, -)
        }.reduce(0, +)
    }

    private func getExteriorSurfaceArea(of cubes: Set<IntPoint3>) -> Int {
        // Bounds
        let minX = cubes.map { $0.x }.min()! - 1
        let minY = cubes.map { $0.y }.min()! - 1
        let minZ = cubes.map { $0.z }.min()! - 1
        let maxX = cubes.map { $0.x }.max()! + 1
        let maxY = cubes.map { $0.y }.max()! + 1
        let maxZ = cubes.map { $0.z }.max()! + 1

        var visitedPoints: Set<IntPoint3> = []
        var toVisit: Set<IntPoint3> = []

        var externalSurfaceArea = 0

        // Start somewhere guaranteed to be outside
        toVisit.insert(IntPoint3(x: minX, y: minY, z: minZ))

        while let point = toVisit.popFirst() {
            visitedPoints.insert(point)

            for offset in neighborOffsets {
                let neighborPoint = point + offset
                guard (minX...maxX).contains(neighborPoint.x) &&
                        (minY...maxY).contains(neighborPoint.y) &&
                        (minZ...maxZ).contains(neighborPoint.z) else {
                    continue
                }

                if cubes.contains(neighborPoint) {
                    externalSurfaceArea += 1
                } else if !visitedPoints.contains(neighborPoint) {
                    toVisit.insert(neighborPoint)
                }
            }
        }

        return externalSurfaceArea
    }

    override func solveFunction1() -> CustomStringConvertible {
        let surfaceArea = getSurfaceArea(of: cubes)
        return "\(surfaceArea)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let surfaceArea = getExteriorSurfaceArea(of: cubes)
        return "\(surfaceArea)"
    }
}
