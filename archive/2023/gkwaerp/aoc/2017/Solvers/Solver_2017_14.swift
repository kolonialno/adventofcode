//
//  Solver_2017_14.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 26/11/2023.
//

import Foundation

final class Solver_2017_14: Solver {
    final class Defragmenter {
        let key: String
        var layout: [String]?

        init(key: String) {
            self.key = key
            self.layout = nil
        }

        private func getBinaryString(for hexString: String) -> String {
            var binaryString = ""
            hexString.forEach { char in
                let sChar = "\(char)"
                let intValue = Int(sChar, radix: 16)!
                var binarySubString = String(intValue, radix: 2)
                while binarySubString.count < 4 {
                    binarySubString = "0\(binarySubString)"
                }
                binaryString.append(binarySubString)
            }

            return binaryString
        }

        func getNumUsed() -> Int {
            let binaryStrings = getLayout()
            return binaryStrings
                .map { $0.filter { $0 == "1" } }
                .map { $0.count }
                .reduce(0, +)
        }

        func getLayout() -> [String] {
            if let layout {
                return layout
            }

            let binaryStrings = (0..<128).map {
                let hashString = "\(key)-\($0)"
                let hasher = Common_2017.KnotHasher(listSize: 256, string: hashString)
                hasher.runHash(numRounds: 64)
                let hexString = hasher.getHexString()
                let binarySubstring = getBinaryString(for: hexString)
                return binarySubstring
            }

            layout = binaryStrings

            return binaryStrings
        }

        func countRegions() -> Int {
            func getValidNeigbors(for point: IntPoint, grid: IntGrid) -> [IntPoint] {
                IntPoint.cardinalOffsets
                    .map { point + $0 }
                    .filter { grid.isWithinBounds($0) }
                    .filter { grid.getValue(at: $0) != 0 }
            }

            let grid = IntGrid(stringArray: getLayout())
            var seen: Set<IntPoint> = []
            var numRegions = 0

            for point in grid.gridPoints {
                let value = grid.getValue(at: point)
                guard value != 0 else { continue }

                let insertion = seen.insert(point)
                guard insertion.inserted else { continue }

                var toVisit: [IntPoint] = getValidNeigbors(for: point, grid: grid)

                while let nextPoint = toVisit.popLast() {
                    let insertion = seen.insert(nextPoint)
                    guard insertion.inserted else { continue }

                    toVisit.append(contentsOf: getValidNeigbors(for: nextPoint, grid: grid))
                }

                numRegions += 1
            }

            return numRegions
        }
    }

    private var defragmenter: Defragmenter!

    override func didLoadFunction() {
        let key = defaultInputFileString.loadAsTextString()
        defragmenter = Defragmenter(key: key)
    }

    override func solveFunction1() -> CustomStringConvertible {
        defragmenter.getNumUsed()
    }

    override func solveFunction2() -> CustomStringConvertible {
        defragmenter.countRegions()
    }
}

extension Solver_2017_14: TestableDay {
    func runTests() {
        let input = "flqrgnkx"
        let defragmenter = Defragmenter(key: input)
        
        let numUsed = defragmenter.getNumUsed()
        let expectedUsed = 8108
        assert(numUsed == expectedUsed)

        let numRegions = defragmenter.countRegions()
        let expectedRegions = 1242
        assert(numRegions == expectedRegions)
    }
}
