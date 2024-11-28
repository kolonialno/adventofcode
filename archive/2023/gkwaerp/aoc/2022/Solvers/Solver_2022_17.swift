//
//  Solver_2022_17.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 17/12/2022.
//

import Foundation

class Solver_2022_17: Solver {
    enum RockType: Int, CaseIterable {
        case horizontalLine
        case cross
        case mirrorL
        case verticalLine
        case box

        var offsets: Set<IntPoint> {
            switch self {
            case .horizontalLine:
                return [IntPoint(x: 0, y: 0), IntPoint(x: 1, y: 0), IntPoint(x: 2, y: 0), IntPoint(x: 3, y: 0)]
            case .cross:
                return [IntPoint(x: 1, y: 0), IntPoint(x: 0, y: 1), IntPoint(x: 1, y: 1), IntPoint(x: 2, y: 1), IntPoint(x: 1, y: 2)]
            case .mirrorL:
                return [IntPoint(x: 2, y: 0), IntPoint(x: 2, y: 1), IntPoint(x: 0, y: 2), IntPoint(x: 1, y: 2), IntPoint(x: 2, y: 2)]
            case .verticalLine:
                return [IntPoint(x: 0, y: 0), IntPoint(x: 0, y: 1), IntPoint(x: 0, y: 2), IntPoint(x: 0, y: 3)]
            case .box:
                return [IntPoint(x: 0, y: 0), IntPoint(x: 1, y: 0), IntPoint(x: 0, y: 1), IntPoint(x: 1, y: 1)]
            }
        }

        var height: Int {
            offsets.max(by: { $0.y < $1.y })!.y
        }

        var width: Int {
            offsets.max(by: { $0.x < $1.x })!.x
        }
    }

    class Rock {
        let rockType: RockType

        /// Top-left of bounding box
        var position: IntPoint

        var top: Int { position.y }
        var left: Int { position.x }
        var right: Int { position.x + rockType.width }
        var bottom: Int { position.y + rockType.height }

        var occupiedSlots: Set<IntPoint> {
            Set(rockType.offsets.map { position + $0 })
        }

        init(position: IntPoint, rockType: RockType) {
            self.position = position
            self.rockType = rockType
        }

        func canMove(in direction: Direction, width: Int, occupiedPositions: Set<IntPoint>) -> Bool {
            // Undo movement
            defer {
                position -= direction.movementVector
            }

            position += direction.movementVector

            if left < 0 || right >= width {
                return false
            }

            if bottom >= 0 {
                return false
            }

            return self.occupiedSlots.isDisjoint(with: occupiedPositions)
        }
    }

    class Rocktris {
        private let jetPattern: [Direction]
        private var jetIndex: Int
        private var rockTypeIndex: Int

        init(string: String) {
            self.jetPattern = string.convertToStringArray().map { Direction(string: $0)! }
            jetIndex = 0
            rockTypeIndex = 0
        }

        private func getRockType() -> RockType {
            let rockType = RockType(rawValue: rockTypeIndex)!
            rockTypeIndex += 1
            rockTypeIndex %= RockType.allCases.count
            return rockType
        }

        private func getJetDirection() -> Direction {
            let jetDirection = jetPattern[jetIndex]
            jetIndex += 1
            jetIndex %= jetPattern.count
            return jetDirection
        }

        func simulate(numRocksToLand: Int) -> Int {
            let width = 7
            var numLandedRocks = 0
            var occupiedPositions: Set<IntPoint> = []
            var heightOfTallest: Int = 0

            // RockType --> JetIndex --> Count
            var history: [Int: [Int: Int]] = [:]

            // RockType --> JetIndex --> Height gained
            var rockJetHeights: [Int: [Int: Int]] = [:]

            // RockType --> Looping jet indices
            var loopingJetIndices: [Int: Set<Int>] = [:]

            // RockType --> Height gained per loop
            var loopHeightValues: [Int: Int] = [:]

            // RockType --> Starting jet index --> End jet index
            var rockJetEndIndices: [Int: [Int: Int]] = [:]

            // Height gained by all rocks looping once
            var fullLoopHeightGain = 0

            var readyToMath = false
            while !readyToMath {
                let startHeight = heightOfTallest
                let startingRockTypeIndex = rockTypeIndex
                let startingJetIndex = jetIndex

                history[startingRockTypeIndex, default: [:]][startingJetIndex, default: 0] += 1
                if history[startingRockTypeIndex]?[startingJetIndex] ?? 0 == 2 {
                    // This combination (rock type + jet index) loops.
                    loopingJetIndices[startingRockTypeIndex, default: []].insert(startingJetIndex)
                } else if history[startingRockTypeIndex]?[startingJetIndex] ?? 0 == 3 {
                    // We have found *all* looping indices now for this rock type
                    let loopingIndices = loopingJetIndices[startingRockTypeIndex]!
                    let loopHeightGain = loopingIndices.map { rockJetHeights[startingRockTypeIndex]![$0]! }.reduce(0, +)
                    loopHeightValues[startingRockTypeIndex] = loopHeightGain

                    if loopHeightValues.count == RockType.allCases.count {
                        fullLoopHeightGain = loopHeightValues.map { $0.value }.reduce(0, +)
                        readyToMath = true
                        continue
                    }
                }

                let rockType = getRockType()
                let startPos = IntPoint(x: 2, y: heightOfTallest - 3 - rockType.height - 1)
                let rock = Rock(position: startPos, rockType: rockType)

                var landed = false
                while !landed {
                    let jetDirection = getJetDirection()

                    if rock.canMove(in: jetDirection, width: width, occupiedPositions: occupiedPositions) {
                        rock.position += jetDirection.movementVector
                    }
                    if rock.canMove(in: .south, width: width, occupiedPositions: occupiedPositions) {
                        rock.position += Direction.south.movementVector
                    } else {
                        landed = true
                        numLandedRocks += 1
                        occupiedPositions.formUnion(rock.occupiedSlots)
                        heightOfTallest = min(heightOfTallest, rock.top)

                        let deltaHeight = heightOfTallest - startHeight
                        rockJetHeights[startingRockTypeIndex, default: [:]][startingJetIndex] = deltaHeight
                        rockJetEndIndices[startingRockTypeIndex, default: [:]][startingJetIndex] = jetIndex

                        // In case we're done before loop detection kicks in
                        if numRocksToLand == numLandedRocks {
                            readyToMath = true
                        }
                    }
                }
            }

            // We are ready to math
            var mathGain = 0
            let numRemainingRocks = numRocksToLand - numLandedRocks

            // Full loop --> all rocks can do their own loop.
            // Assumes every rock loops in same amount of time. I *think* this must be the case, but not sure how to prove/disprove.
            let numRocksForFullLoop = RockType.allCases.count * loopingJetIndices[0]!.count
            let fullRockLoops = numRemainingRocks / numRocksForFullLoop
            let remainderRocks = numRemainingRocks % numRocksForFullLoop

            mathGain += fullRockLoops * fullLoopHeightGain

            for _ in 0..<remainderRocks {
                mathGain += rockJetHeights[rockTypeIndex]![jetIndex]!
                jetIndex = rockJetEndIndices[rockTypeIndex]![jetIndex]!
                _ = getRockType()
            }

            heightOfTallest += mathGain

            return -heightOfTallest
        }
    }

    private var input = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let rocktris = Rocktris(string: input)
        let result = rocktris.simulate(numRocksToLand: 2022)
        return "\(result)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let rocktris = Rocktris(string: input)
        let result = rocktris.simulate(numRocksToLand: 1000000000000)
        return "\(result)"
    }
}
