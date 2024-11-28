//
//  Solver_2017_17.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 27/11/2023.
//

import Foundation

final class Solver_2017_17: Solver {
    struct Spinlock {
        let stepSize: Int

        init(stepSize: Int) {
            self.stepSize = stepSize
        }

        func spin() -> Int {
            var buffer = [0]
            var currentPosition = 0
            for i in 1...2017 {
                let insertionPosition = ((currentPosition + stepSize) % buffer.count) + 1
                buffer.insert(i, at: insertionPosition)
                currentPosition = insertionPosition
            }

            let index = (buffer.firstIndex(where: { $0 == 2017 })! + 1) % buffer.count
            return buffer[index]
        }

        func spinQuick() -> Int {
            var result = 0
            var currentPosition = 0

            for i in 1...50_000_000 {
                currentPosition = ((currentPosition + stepSize) % i) + 1
                if currentPosition == 1 {
                    result = i
                }
            }

            return result
        }
    }

    private var stepSize = 0

    override func didLoadFunction() {
        stepSize = defaultInputFileString.loadAsTextString().intValue!
    }

    override func solveFunction1() -> CustomStringConvertible {
        let spinlock = Spinlock(stepSize: stepSize)
        return spinlock.spin()
    }

    override func solveFunction2() -> CustomStringConvertible {
        let spinlock = Spinlock(stepSize: stepSize)
        return spinlock.spinQuick()
    }
}

extension Solver_2017_17: TestableDay {
    func runTests() {
        let spinlock = Spinlock(stepSize: 3)
        let result = spinlock.spin()
        let expected = 638
        assert(result == expected)
    }
}
