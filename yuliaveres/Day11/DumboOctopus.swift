//
//  DumboOctopus.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 11/12/2021.
//

import Foundation

struct DumboOctopus {

    func part1(_ input: String) -> Int {
        var lines = input.components(separatedBy: "\n").map { $0.compactMap { Int(String($0)) } }

        return check(&lines, times: 100, breakAtFirstSimultaneousFlash: false)
    }

    func part2(_ input: String) -> Int {
        var lines = input.components(separatedBy: "\n").map { $0.compactMap { Int(String($0)) } }

        return check(&lines, times: 1000, breakAtFirstSimultaneousFlash: true)
    }

    private func check(_ matrix: inout [[Int]], times: Int, breakAtFirstSimultaneousFlash: Bool) -> Int {
        var result = 0

        var flashed = Set<FlashInfo>()

        for loop in 1...times {

            var flashedInThisLoop = Set<FlashInfo>()

            for y in 0...matrix.count - 1 {

                for x in 0...matrix[y].count - 1 {

                    matrix[y][x] += 1

                    let position = Position(x: x, y: y)
                    let flashInfo = FlashInfo(position: position, loop: loop)
                    if matrix[y][x] > 9 && !flashedInThisLoop.contains(flashInfo) {
                        flashedInThisLoop.insert(flashInfo)

                        increaseAllAdjacentBy1(in: &matrix, at: position, loop: loop, flashed: &flashedInThisLoop)
                    }
                }
            }

            for flashed in flashedInThisLoop {
                matrix[flashed.position.y][flashed.position.x] = 0
            }

            if flashedInThisLoop.count == matrix.count * matrix[0].count {
                result = loop
                break
            }

            flashed.formUnion(flashedInThisLoop)
        }

        if !breakAtFirstSimultaneousFlash {
            result = flashed.count
        }

        return result
    }

    private func increaseAllAdjacentBy1(in matrix: inout [[Int]], at position: Position, loop: Int, flashed: inout Set<FlashInfo>) {
        let x = position.x
        let y = position.y

        if y > 0 {
            matrix[y - 1][x] += 1
            let adjacentPosition = Position(x: x, y: y - 1)
            let flashInfo = FlashInfo(position: adjacentPosition, loop: loop)
            if matrix[y - 1][x] > 9 && !flashed.contains(flashInfo) {
                flashed.insert(flashInfo)
                increaseAllAdjacentBy1(in: &matrix, at: adjacentPosition, loop: loop, flashed: &flashed)
            }

            if x < matrix[y].count - 1 {
                matrix[y - 1][x + 1] += 1
                let adjacentPosition = Position(x: x + 1, y: y - 1)
                let flashInfo = FlashInfo(position: adjacentPosition, loop: loop)
                if matrix[y - 1][x + 1] > 9 && !flashed.contains(flashInfo) {
                    flashed.insert(flashInfo)
                    increaseAllAdjacentBy1(in: &matrix, at: adjacentPosition, loop: loop, flashed: &flashed)
                }
            }

            if x > 0 {
                matrix[y - 1][x - 1] += 1
                let adjacentPosition = Position(x: x - 1, y: y - 1)
                let flashInfo = FlashInfo(position: adjacentPosition, loop: loop)
                if matrix[y - 1][x - 1] > 9 && !flashed.contains(flashInfo) {
                    flashed.insert(flashInfo)
                    increaseAllAdjacentBy1(in: &matrix, at: adjacentPosition, loop: loop, flashed: &flashed)
                }
            }
        }

        if x < matrix[y].count - 1 {
            matrix[y][x + 1] += 1
            let adjacentPosition = Position(x: x + 1, y: y)
            let flashInfo = FlashInfo(position: adjacentPosition, loop: loop)
            if matrix[y][x + 1] > 9 && !flashed.contains(flashInfo) {
                flashed.insert(flashInfo)
                increaseAllAdjacentBy1(in: &matrix, at: adjacentPosition, loop: loop, flashed: &flashed)
            }
        }

        if x > 0 {
            matrix[y][x - 1] += 1
            let adjacentPosition = Position(x: x - 1, y: y)
            let flashInfo = FlashInfo(position: adjacentPosition, loop: loop)
            if matrix[y][x - 1] > 9 && !flashed.contains(flashInfo) {
                flashed.insert(flashInfo)
                increaseAllAdjacentBy1(in: &matrix, at: adjacentPosition, loop: loop, flashed: &flashed)
            }
        }

        if y < matrix.count - 1 {
            matrix[y + 1][x] += 1
            let adjacentPosition = Position(x: x, y: y + 1)
            let flashInfo = FlashInfo(position: adjacentPosition, loop: loop)
            if matrix[y + 1][x] > 9 && !flashed.contains(flashInfo) {
                flashed.insert(flashInfo)
                increaseAllAdjacentBy1(in: &matrix, at: adjacentPosition, loop: loop, flashed: &flashed)
            }

            if x < matrix[y].count - 1 {
                matrix[y + 1][x + 1] += 1
                let adjacentPosition = Position(x: x + 1, y: y + 1)
                let flashInfo = FlashInfo(position: adjacentPosition, loop: loop)
                if matrix[y + 1][x + 1] > 9 && !flashed.contains(flashInfo) {
                    flashed.insert(flashInfo)
                    increaseAllAdjacentBy1(in: &matrix, at: adjacentPosition, loop: loop, flashed: &flashed)
                }
            }

            if x > 0 {
                matrix[y + 1][x - 1] += 1
                let adjacentPosition = Position(x: x - 1, y: y + 1)
                let flashInfo = FlashInfo(position: adjacentPosition, loop: loop)
                if matrix[y + 1][x - 1] > 9 && !flashed.contains(flashInfo) {
                    flashed.insert(flashInfo)
                    increaseAllAdjacentBy1(in: &matrix, at: adjacentPosition, loop: loop, flashed: &flashed)
                }
            }
        }
    }
}

extension DumboOctopus {

    struct Position: Hashable {
        let x: Int
        let y: Int
    }

    struct FlashInfo: Hashable {
        let position: Position
        let loop: Int

        static var stub: FlashInfo {
            return FlashInfo(position: Position(x: 0, y: 0), loop: 0)
        }
    }

    func printMatrix(_ matrix: [[Int]]) {
        print("\n:")
        for line in matrix {
            print(line.map { String($0) }.joined(separator: ""))
        }
    }
}
