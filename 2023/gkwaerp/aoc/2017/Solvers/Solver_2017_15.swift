//
//  Solver_2017_15.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 27/11/2023.
//

import Foundation

final class Solver_2017_15: Solver {
    private var startingValues: [Int] = []

    override func didLoadFunction() {
        startingValues = defaultInputFileString.loadAsStringArray().map { $0.components(separatedBy: " ").last!.intValue! }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let generators = [Generator(factor: 16807, previousValue: startingValues[0], multipleCriteria: nil),
                          Generator(factor: 48271, previousValue: startingValues[1], multipleCriteria: nil)]

        return getMatching(generatorA: generators[0],
                           generatorB: generators[1],
                           numRounds: 40_000_000)
    }

    override func solveFunction2() -> CustomStringConvertible {
        let generators = [Generator(factor: 16807, previousValue: startingValues[0], multipleCriteria: 4),
                          Generator(factor: 48271, previousValue: startingValues[1], multipleCriteria: 8)]

        return getMatching(generatorA: generators[0],
                           generatorB: generators[1],
                           numRounds: 5_000_000)
    }

    final class Generator {
        let factor: Int
        var previousValue: Int
        let multipleCriteria: Int?

        init(factor: Int, previousValue: Int, multipleCriteria: Int?) {
            self.factor = factor
            self.previousValue = previousValue
            self.multipleCriteria = multipleCriteria
        }

        func generateNextNumber() -> Int {
            var done = false
            var nextValue: Int!
            while !done {
                nextValue = previousValue * factor
                nextValue %= 2147483647

                previousValue = nextValue

                if let multipleCriteria {
                    done = nextValue.isMultiple(of: multipleCriteria)
                } else {
                    done = true
                }
            }

            return nextValue
        }
    }

    func getMatching(generatorA: Generator, generatorB: Generator, numRounds: Int) -> Int {
        var numMatching = 0
        for _ in 0..<numRounds {
            let valueA = generatorA.generateNextNumber() & 0xffff
            let valueB = generatorB.generateNextNumber() & 0xffff

            if valueA == valueB {
                numMatching += 1
            }
        }

        return numMatching
    }
}


extension Solver_2017_15: TestableDay {
    func runTests() {
        let generators = [Generator(factor: 16807, previousValue: 65, multipleCriteria: nil),
                          Generator(factor: 48271, previousValue: 8921, multipleCriteria: nil)]

        let expectedValues = [[1092455, 430625591],
                              [1181022009, 1233683848],
                              [245556042, 1431495498],
                              [1744312007, 137874439],
                              [1352636452, 285222916]]

        for valuePair in expectedValues {
            let generatedValueA = generators[0].generateNextNumber()
            let generatedValueB = generators[1].generateNextNumber()

            assert(generatedValueA == valuePair[0])
            assert(generatedValueB == valuePair[1])
        }


        let generators2 = [Generator(factor: 16807, previousValue: 65, multipleCriteria: nil),
                           Generator(factor: 48271, previousValue: 8921, multipleCriteria: nil)]

        let numMatching = getMatching(generatorA: generators2[0], generatorB: generators2[1], numRounds: 5)
        let expectedMatching = 1
        assert(numMatching == expectedMatching)


        let generators3 = [Generator(factor: 16807, previousValue: 65, multipleCriteria: nil),
                           Generator(factor: 48271, previousValue: 8921, multipleCriteria: nil)]

        let numMatching2 = getMatching(generatorA: generators3[0], generatorB: generators3[1], numRounds: 40_000_000)
        let expectedMatching2 = 588
        assert(numMatching2 == expectedMatching2)

        let generators4 = [Generator(factor: 16807, previousValue: 65, multipleCriteria: 4),
                           Generator(factor: 48271, previousValue: 8921, multipleCriteria: 8)]
        let numMatching3 = getMatching(generatorA: generators4[0], generatorB: generators4[1], numRounds: 5_000_000)
        let expectedMatching3 = 309
        assert(numMatching3 == expectedMatching3)
    }
}
