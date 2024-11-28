//
//  SmokeBasin.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 09/12/2021.
//

import Foundation
import UIKit

struct SmokeBasin {

    func part1(_ input: String) -> Int {
        let matrix = input.components(separatedBy: "\n").map { $0.compactMap { Int(String($0)) } }

        var lowPoints: [Int] = []

        for (lineIdx, line) in matrix.enumerated() {


            for (itemIdx, item) in line.enumerated() {

                let top: Int?
                let left: Int?
                let right: Int?
                let bottom: Int?

                if itemIdx > 0 {
                    left = line[itemIdx - 1]
                } else {
                    left = nil
                }

                if itemIdx < line.count - 1 {
                    right = line[itemIdx + 1]
                } else {
                    right = nil
                }

                if lineIdx > 0 {
                    top = matrix[lineIdx - 1][itemIdx]
                } else {
                    top = nil
                }

                if lineIdx < matrix.count - 1 {
                    bottom = matrix[lineIdx + 1][itemIdx]
                } else {
                    bottom = nil
                }

                var isLowPoint: Bool = true
                let adjacents = [top, left, bottom, right].compactMap { $0 }
                for adjacent in adjacents {
                    if item >= adjacent {
                        isLowPoint = false
                        break
                    }
                }

                if isLowPoint {
                    lowPoints.append(item)
                }
            }
        }

        let result = lowPoints.map { $0 + 1 }.reduce(0, +)
        
        return result
    }

    struct Point: Hashable {
        let x: Int
        let y: Int
    }

    func part2(_ input: String) -> Int {
        let matrix = input.components(separatedBy: "\n").map { $0.compactMap { Int(String($0)) } }

        var sizes: [Int] = []

        for (lineIdx, line) in matrix.enumerated() {

            for (itemIdx, item) in line.enumerated() {

                let top: Int?
                let left: Int?
                let right: Int?
                let bottom: Int?

                if itemIdx > 0 {
                    left = line[itemIdx - 1]
                } else {
                    left = nil
                }

                if itemIdx < line.count - 1 {
                    right = line[itemIdx + 1]
                } else {
                    right = nil
                }

                if lineIdx > 0 {
                    top = matrix[lineIdx - 1][itemIdx]
                } else {
                    top = nil
                }

                if lineIdx < matrix.count - 1 {
                    bottom = matrix[lineIdx + 1][itemIdx]
                } else {
                    bottom = nil
                }

                var isLowPoint: Bool = true
                let adjacents = [top, left, bottom, right].compactMap { $0 }

                for adjacent in adjacents {
                    if item >= adjacent {
                        isLowPoint = false
                        break
                    }
                }

                var basinPoints = Set<Point>()
                if isLowPoint {
                    basinPoints = basinSize(for: Point(x: itemIdx, y: lineIdx), matrix: matrix)
                }

                if !basinPoints.isEmpty {
                    sizes.append(basinPoints.count)
                }
            }
        }

        sizes.sort(by: { $0 > $1 })
        let result = sizes.prefix(3).reduce(1, *)

        return result
    }

    private func basinSize(for point: Point, matrix: [[Int]]) -> Set<Point> {
        let top: Point?
        let left: Point?
        let right: Point?
        let bottom: Point?

        if point.x > 0 {
            left = Point(x: point.x - 1, y: point.y)
        } else {
            left = nil
        }

        if point.x < matrix[point.y].count - 1 {
            right = Point(x: point.x + 1, y: point.y)
        } else {
            right = nil
        }

        if point.y > 0 {
            top = Point(x: point.x, y: point.y - 1)
        } else {
            top = nil
        }

        if point.y < matrix.count - 1 {
            bottom = Point(x: point.x, y: point.y + 1)
        } else {
            bottom = nil
        }

        var basinPoints = Set<Point>()
        basinPoints.insert(point)

        let adjacents = [top, left, bottom, right].compactMap { $0 }

        for adjacent in adjacents {

            let adjacentValue = matrix[adjacent.y][adjacent.x]
            let currentPositionValue = matrix[point.y][point.x]

            if currentPositionValue < adjacentValue && adjacentValue != 9 {
                let next = basinSize(for: adjacent, matrix: matrix)
                basinPoints.formUnion(next)
            }
        }

        return basinPoints
    }
}
