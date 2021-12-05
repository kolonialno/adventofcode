//
//  HydrothermalVenture.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 05/12/2021.
//

import Foundation

struct HydrothermalVenture {

    func part1(_ input: String) throws -> Int {
        let vectors = Parser().parse(input)
        let map = pointsMap(for: vectors, line: [.straight])

        return frequentOccurrencePointsCount(in: map)
    }

    func part2(_ input: String) throws -> Int {
        let vectors = Parser().parse(input)
        let map = pointsMap(for: vectors, line: .all)

        return frequentOccurrencePointsCount(in: map)
    }

    func pointsMap(for vectors: [(Point, Point)], line: Line) -> [Point: Int] {
        var map: [Point: Int] = [:]

        for vector in vectors {

            if vector.0.x == vector.1.x {

                let min = min(vector.0.y, vector.1.y)
                let max = max(vector.0.y, vector.1.y)
                for y in min...max {
                    let point = Point(x: vector.0.x, y: y)
                    if let existing = map[point] {
                        map[point] = existing + 1
                    } else {
                        map[point] = 1
                    }
                }
            } else if vector.0.y == vector.1.y {

                let min = min(vector.0.x, vector.1.x)
                let max = max(vector.0.x, vector.1.x)
                for x in min...max {
                    let point = Point(x: x, y: vector.0.y)
                    if let existing = map[point] {
                        map[point] = existing + 1
                    } else {
                        map[point] = 1
                    }
                }
            } else if line.contains(.diagonal) {

                let x1 = vector.0.x
                let x2 = vector.1.x

                let y1 = vector.0.y
                let y2 = vector.1.y

                let xStride = x1 < x2 ? 1 : -1
                let yStride = y1 < y2 ? 1 : -1

                for (x, y) in zip(stride(from: x1, to: x2 + xStride, by: xStride),
                                  stride(from: y1, to: y2 + yStride, by: yStride)) {
                    let point = Point(x: x, y: y)
                    if let existing = map[point] {
                        map[point] = existing + 1
                    } else {
                        map[point] = 1
                    }
                }
            } else {
                continue
            }
        }

        return map
    }

    private func frequentOccurrencePointsCount(in map: [Point: Int]) -> Int {
        return map.filter { $0.value > 1 }.count
    }
}

extension HydrothermalVenture {

    struct Line: OptionSet {
        let rawValue: Int

        static let straight: Line = Line(rawValue: 1 << 0)
        static let diagonal: Line = Line(rawValue: 1 << 1)

        static let all: Line = [.straight, .diagonal]
    }

    struct Point: Hashable {
        let x: Int
        let y: Int
    }

    struct Parser {

        func parse(_ input: String) -> [(Point, Point)] {
            let lines = input.components(separatedBy: "\n")
            return lines.map { pairString -> (Point, Point) in

                let pair = pairString.components(separatedBy: " -> ")
                let start = pair[0].components(separatedBy: ",")
                let end = pair[1].components(separatedBy: ",")

                let startPoint = Point(x: Int(start[0])!, y: Int(start[1])!)
                let endPoint = Point(x: Int(end[0])!, y: Int(end[1])!)

                return (startPoint, endPoint)
            }
        }
    }
}
