//
//  Solver_2017_06.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 14/11/2023.
//

import Foundation

final class Solver_2017_06: Solver {

    struct LoopInfo {
        let numSteps: Int
        let size: Int
    }

    private var input: [Int] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsIntArray(separator: .whitespaces)
    }

    override func solveFunction1() -> CustomStringConvertible {
        let result = getLoopInfo(input).numSteps
        return result
    }

    override func solveFunction2() -> CustomStringConvertible {
        let result = getLoopInfo(input).size
        return result
    }

    private func getLoopInfo(_ input: [Int]) -> LoopInfo {
        func findLoop(_ banks: inout [Int]) -> Int {
            let numBanks = banks.count
            var history: Set<[Int]> = [banks]

            while true {
                let highestAmount = banks.max()!
                let bankIndexToRedistribute = banks.firstIndex(where: { $0 == highestAmount })!

                let blocksToRedistribute = banks[bankIndexToRedistribute]
                banks[bankIndexToRedistribute] = 0

                let fullCycles = blocksToRedistribute / numBanks
                for i in 0..<numBanks {
                    banks[i] += fullCycles
                }
                let remainder = blocksToRedistribute % numBanks

                for i in 0..<remainder {
                    let bankIndex = (bankIndexToRedistribute + 1 + i) % numBanks
                    banks[bankIndex] += 1
                }

                let result = history.insert(banks)
                if !result.inserted {
                    return history.count
                }
            }
        }

        var banks = input
        let numSteps = findLoop(&banks)
        let size = findLoop(&banks)

        return LoopInfo(numSteps: numSteps, size: size)
    }
}

extension Solver_2017_06: TestableDay {
    func runTests() {
        let input = [0, 2, 7, 0]
        let loopInfo = getLoopInfo(input)
        let expectedSteps = 5
        let expectedSize = 4
        assert(loopInfo.numSteps == expectedSteps)
        assert(loopInfo.size == expectedSize)
    }
}
