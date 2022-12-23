//
//  Solver_2022_23.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 23/12/2022.
//

import Foundation

class Solver_2022_23: Solver {
    class GameOfElf {
        class Elf: Equatable, Hashable {
            let id = UUID().uuidString

            static func ==(lhs: Elf, rhs: Elf) -> Bool {
                lhs.id == rhs.id
            }

            func hash(into hasher: inout Hasher) {
                hasher.combine(id)
            }
        }

        private var areaMap: [IntPoint: Elf] = [:]
        private var directionOffset = 0

        init(strings: [String]) {
            for (y, row) in strings.enumerated() {
                for (x, char) in row.enumerated() {
                    if char == "#" {
                        areaMap[IntPoint(x: x, y: y)] = Elf()
                    }
                }
            }
        }

        /// Offfsets to check, first is always direction to move if all clear
        private func getDirectionalOffsetsToCheck(attempt: Int) -> [IntPoint] {
            let directions: [[IntPoint]] = [[IntPoint(north: 1), IntPoint(north: 1, east: 1), IntPoint(north: 1, west: 1)],
                                            [IntPoint(south: 1), IntPoint(south: 1, east: 1), IntPoint(south: 1, west: 1)],
                                            [IntPoint(west: 1), IntPoint(north: 1, west: 1), IntPoint(south: 1, west: 1)],
                                            [IntPoint(east: 1), IntPoint(north: 1, east: 1), IntPoint(south: 1, east: 1)]]
            let actualIndex = (attempt + directionOffset) % 4
            return directions[actualIndex]
        }

        enum SimulationMode {
            case setRounds(numRounds: Int)
            case bragging
        }

        func simulate(mode: SimulationMode) -> Int {
            let allDirectionOffsets = IntPoint.allDirectionOffsets

            let numRounds: Int
            switch mode {
            case .setRounds(let numRoundsToDo):
                numRounds = numRoundsToDo
            case .bragging:
                numRounds = Int.max
            }

            for round in (1...numRounds) {
                var proposedPositions: [IntPoint: Set<Elf>] = [:]

                // Propose new locations
                areaMap.forEach { (position, elf) in
                    guard !allDirectionOffsets.allSatisfy({ areaMap[position + $0] == nil }) else {
                        return
                    }

                    for i in (0..<4) {
                        let offsetsToCheck = getDirectionalOffsetsToCheck(attempt: i)
                        if offsetsToCheck.allSatisfy({ areaMap[position + $0] == nil }) {
                            proposedPositions[position + offsetsToCheck.first!, default: []].insert(elf)
                            break
                        }
                    }
                }

                directionOffset += 1

                // If no one wants to move
                if proposedPositions.isEmpty {
                    if case .bragging = mode {
                        return round
                    } else {
                        break
                    }
                }

                // Move if only one elf proposed location
                proposedPositions
                    .filter { $0.value.count == 1 }
                    .forEach { (position, elves) in
                        let elf = elves.first!
                        let elfPos = areaMap.first(where: { $0.value == elf })!.key
                        areaMap[elfPos] = nil
                        areaMap[position] = elf
                    }
            }

            let minX = areaMap.keys.map { $0.x }.min()!
            let maxX = areaMap.keys.map { $0.x }.max()!
            let minY = areaMap.keys.map { $0.y }.min()!
            let maxY = areaMap.keys.map { $0.y }.max()!

            let width = maxX - minX + 1
            let height = maxY - minY + 1
            let area = width * height
            return area - areaMap.count
        }
    }

    override func solveFunction1() -> String {
        let input = defaultInputFileString.loadAsTextStringArray()
        let gameOfElf = GameOfElf(strings: input)
        let emptyGroundTiles = gameOfElf.simulate(mode: .setRounds(numRounds: 10))
        return "\(emptyGroundTiles)"
    }

    override func solveFunction2() -> String {
        let input = defaultInputFileString.loadAsTextStringArray()
        let gameOfElf = GameOfElf(strings: input)
        let roundsToBragAbout = gameOfElf.simulate(mode: .bragging)
        return "\(roundsToBragAbout)"
    }
}

extension Solver_2022_23: TestableDay {
    func runTests() {
        let testInputA = defaultTestInputString(suffix: "a").loadAsTextStringArray()

        let game1 = GameOfElf(strings: testInputA)
        let result1 = game1.simulate(mode: .setRounds(numRounds: 10))
        assert(result1 == 110)

        let game2 = GameOfElf(strings: testInputA)
        let result2 = game2.simulate(mode: .bragging)
        assert(result2 == 20)
    }
}
