//
//  IntPoint.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class IntPoint: Equatable, Hashable {
    var x: Int
    var y: Int

    init(x: Int, y: Int) {
        self.x = x
        self.y = y
    }

    /// Convenience -- only set 1 of north/south and 1 of east/west
    init(north: Int = 0, south: Int = 0, east: Int = 0, west: Int = 0) {
        guard north * south == 0 else {
            fatalError("Invalid north/south, only 1 can be set.")
        }

        guard east * west == 0 else {
            fatalError("Invalid east/west, only 1 can be set.")
        }

        self.x = east != 0 ? east : -west
        self.y = north != 0 ? -north : south
    }

    static func == (lhs: IntPoint, rhs: IntPoint) -> Bool {
        lhs.x == rhs.x && lhs.y == rhs.y
    }

    func copy() -> IntPoint {
        IntPoint(x: x, y: y)
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
    }

    static var origin: IntPoint {
        IntPoint(x: 0, y: 0)
    }

    func manhattanDistance(to other: IntPoint = .origin) -> Int {
        abs(x - other.x) + abs(y - other.y)
    }

    func chebyshevDistance(to other: IntPoint = .origin) -> Int {
        max(abs(x - other.x), abs(y - other.y))
    }

    func scaled(by scalar: Int) -> IntPoint {
        scaled(xScalar: scalar, yScalar: scalar)
    }

    func scaled(xScalar: Int, yScalar: Int) -> IntPoint {
        IntPoint(x: x * xScalar, y: y * yScalar)
    }

    func divided(by scalar: Int) -> IntPoint {
        divided(xScalar: scalar, yScalar: scalar)
    }

    func divided(xScalar: Int, yScalar: Int) -> IntPoint {
        IntPoint(x: x / xScalar, y: y / yScalar)
    }

    func move(in direction: Direction, numSteps: Int) -> IntPoint {
        self + direction.movementVector.scaled(by: numSteps)
    }

    func magnitude() -> Int {
        abs(x) + abs(y)
    }

    func rotate(around point: IntPoint, left: Bool) -> IntPoint {
        let delta = self - point
        let rotated = left ? IntPoint(x: delta.y, y: -delta.x) : IntPoint(x: -delta.y, y: delta.x)
        return rotated + point
    }

    static func +(lhs: IntPoint, rhs: IntPoint) -> IntPoint {
        IntPoint(x: lhs.x + rhs.x, y: lhs.y + rhs.y)
    }

    static func -(lhs: IntPoint, rhs: IntPoint) -> IntPoint {
        IntPoint(x: lhs.x - rhs.x, y: lhs.y - rhs.y)
    }

    static func +=(lhs: inout IntPoint, rhs: IntPoint) {
        lhs = lhs + rhs
    }

    static func -=(lhs: inout IntPoint, rhs: IntPoint) {
        lhs = lhs - rhs
    }

    static func angle(between a: IntPoint, and b: IntPoint, invertY: Bool) -> Double? {
        guard a != b else {
            return nil
        }

        let delta = b - a
        let x = Double(delta.x)
        let y = invertY ? Double(-delta.y) : Double(delta.y)

        return atan2(x, y)
    }

    /// Returns all points in line between `a` & `b` -- `[a...b]`.
    /// Must be able to step in direct line (with direction.x & direction.y both being 0 or ± 1)
    static func line(from a: IntPoint, to b: IntPoint) -> [IntPoint] {
        let delta = b - a
        let distance = delta.chebyshevDistance()

        let absx = abs(delta.x)
        let absy = abs(delta.y)
        assert(absx.isMultiple(of: distance))
        assert(absy.isMultiple(of: distance))

        let direction = IntPoint(x: delta.x.signum(), y: delta.y.signum())
        return (0...distance).map { a + direction.scaled(by: $0) }
    }

    static func angleInDegrees(between a: IntPoint, and b: IntPoint, invertY: Bool) -> Double? {
        guard let radians = IntPoint.angle(between: a, and: b, invertY: invertY) else {
            return nil
        }

        return (radians * 180.0 / Double.pi)
    }

    static func gridInfo<T: Collection>(from coordinates: T) -> GridInfo where T.Element: IntPoint {
        guard coordinates.count > 0 else {
            return GridInfo.zero
        }

        let minExtents = coordinates.first!.copy()
        let maxExtents = coordinates.first!.copy()

        for point in coordinates {
            let intPoint = point as IntPoint

            minExtents.x = min(minExtents.x, intPoint.x)
            maxExtents.x = max(maxExtents.x, intPoint.x)
            minExtents.y = min(minExtents.y, intPoint.y)
            maxExtents.y = max(maxExtents.y, intPoint.y)
        }

        return GridInfo(minExtents: minExtents, maxExtents: maxExtents)
    }

    lazy var gridPoints: [IntPoint] = {
        var allPoints = [IntPoint]()
        for yPos in 0..<y {
            for xPos in 0..<x {
                allPoints.append(IntPoint(x: xPos, y: yPos))
            }
        }
        return allPoints
    }()

    static var cardinalOffsets: [IntPoint] {
        Direction.allCases.map { $0.movementVector }
    }

    static var diagonalOffsets: [IntPoint] {
        [IntPoint(x: 1, y: 1),
         IntPoint(x: 1, y: -1),
         IntPoint(x: -1, y: -1),
         IntPoint(x: -1, y: 1)]
    }

    /// Cardinal + Diagonal
    static var allDirectionOffsets: [IntPoint] {
        cardinalOffsets + diagonalOffsets
    }
}

extension IntPoint: CustomStringConvertible {
    var description: String {
        "X: \(x), Y: \(y)"
    }
}
