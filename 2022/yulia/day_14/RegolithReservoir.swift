//
//  RegolithReservoir.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 14/12/2022.
//

import Foundation

struct RegolithReservoir {

    struct Position: Hashable {
        let x: Int
        let y: Int

        var description: String {
            return "\(x),\(y)"
        }
    }

    func part1(_ input: String, hasInfiniteFloor: Bool) -> Int {
        let rocks = rockPaths(from: input)
        let lowestPresentRock = rocks.sorted(by: { $0.y > $1.y }).first!
        let lowestRock = hasInfiniteFloor ? Position(x: 0, y: lowestPresentRock.y + 2) : lowestPresentRock
        var landedSands = Set<Position>()

        var inVoid: Bool = false
        while !inVoid {

            var hasLanded: Bool = false
            var position: Position = .init(x: 500, y: 0)

            while !hasLanded {
                let nextPosition: Position = .init(x: position.x, y: position.y + 1)
                let result = self.checkNextPosition(nextPosition,
                                                    rocks: rocks,
                                                    landedSands: landedSands,
                                                    lowestRock: lowestRock,
                                                    hasInfiniteFloor: hasInfiniteFloor)

                switch result {
                case .proceed(let validPosition):
                    position = validPosition
                case .landed:
                    hasLanded = true
                    landedSands.insert(position)

                    if hasInfiniteFloor && position.x == 500 && position.y == 0 {
                        inVoid = true
                    }
                case .fallingOut:
                    inVoid = true
                    hasLanded = true
                }
            }
        }

        return landedSands.count
    }

    enum NextPosition {
        case proceed(Position)
        case landed
        case fallingOut
    }

    private func checkNextPosition(_ next: Position,
                                   rocks: Set<Position>,
                                   landedSands: Set<Position>,
                                   lowestRock: Position,
                                   hasInfiniteFloor: Bool) -> NextPosition {
        if !rocks.contains(next) && !landedSands.contains(next) {
            if next.y < lowestRock.y {
                return .proceed(next)
            } else {
                return hasInfiniteFloor ? .landed : .fallingOut
            }
        } else {
            let left = Position(x: next.x - 1, y: next.y)
            if !rocks.contains(left) && !landedSands.contains(left) {
                if left.y < lowestRock.y {
                    return .proceed(left)
                } else {
                    return hasInfiniteFloor ? .landed : .fallingOut
                }
            } else {
                let right = Position(x: next.x + 1, y: next.y)
                if !rocks.contains(right) && !landedSands.contains(right) {
                    if right.y < lowestRock.y {
                        return .proceed(right)
                    } else {
                        return hasInfiniteFloor ? .landed : .fallingOut
                    }
                } else {
                    // landed
                    return .landed
                }
            }
        }
    }

    private func visualise(rocks: Set<Position>, landedSands: Set<Position>) {
        for y in Array(0...9) {
            var line: String = ""
            for x in Array(494...503) {
                let char: String
                if rocks.contains(.init(x: x, y: y)) {
                    char = "#"
                } else if landedSands.contains(.init(x: x, y: y)) {
                    char = "o"
                } else {
                    char = "."
                }

                line.append(char)
            }

            print(line)
        }
    }

    private func rockPaths(from input: String) -> Set<Position> {
        let rockLines = input.components(separatedBy: "\n")
        var rocksPositions = Set<Position>()
        for line in rockLines {
            let points = line.components(separatedBy: " -> ").map { pStr -> Position in
                let comp = pStr.components(separatedBy: ",")
                return .init(x: Int(comp[0])!, y: Int(comp[1])!)
            }

            var positions = Set<Position>()
            for (idx, point) in points.enumerated() {
                guard idx + 1 <= points.count - 1 else { break }

                let next = points[idx + 1]

                let points: Set<Position>
                if point.x == next.x {
                    let yrange = point.y > next.y ? Array(next.y...point.y) : Array(point.y...next.y)
                    points = Set(yrange.map { .init(x: point.x, y: $0) })
                } else if point.y == next.y {
                    let xrange = point.x > next.x ? Array(next.x...point.x) : Array(point.x...next.x)
                    points = Set(xrange.map { .init(x: $0, y: point.y) })
                } else {
                    fatalError("Unexpected")
                }

                positions.formUnion(points)
            }

            rocksPositions.formUnion(positions)
        }

        return rocksPositions
    }
}
