//
//  Solver_2022_09.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 09/12/2022.
//

import Foundation

class Solver_2022_09: Solver {
    class Simulator {
        /// `first` is head, `last` is tail
        private var knotPositions: [IntPoint]
        private var tailVisits: Set<IntPoint>

        init(numKnots: Int) {
            self.knotPositions = (0..<numKnots).map { _ in .origin }
            self.tailVisits = [.origin]
        }

        func simulate(input: [String]) -> Int {
            input.forEach { string in
                let split = string.components(separatedBy: " ")
                let direction = Direction(string: split[0])!
                let steps = Int(split[1])!

                simulateStep(direction: direction, steps: steps)
            }

            return tailVisits.count
        }

        private func simulateStep(direction: Direction, steps: Int) {
            (0..<steps).forEach { _ in
                knotPositions[0] += direction.movementVector

                for i in 1..<knotPositions.count {
                    guard updatePositions(knotIndex: i) else {
                        break
                    }
                }

                tailVisits.insert(knotPositions.last!)
            }
        }

        /// Returns whether knot moved (which could trigger next knot to move)
        private func updatePositions(knotIndex i: Int) -> Bool {
            let delta = knotPositions[i - 1] - knotPositions[i]

            // Not separated enough to force a move, no need to check more knots
            guard delta.chebyshevDistance() > 1 else {
                return false
            }

            knotPositions[i] += IntPoint(x: delta.x.signum(), y: delta.y.signum())

            return true
        }
    }

    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let simulator = Simulator(numKnots: 2)
        let numVisited = simulator.simulate(input: input)
        return "\(numVisited)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let simulator = Simulator(numKnots: 10)
        let numVisited = simulator.simulate(input: input)
        return "\(numVisited)"
    }
}

extension Solver_2022_09: TestableDay {
    func runTests() {
        let testInput1 = """
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""
            .components(separatedBy: .newlines)
            .filter { !$0.isEmpty }

        let testInput2 = """
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"""
            .components(separatedBy: .newlines)
            .filter { !$0.isEmpty }

        let simulator = Simulator(numKnots: 2)
        assert(simulator.simulate(input: testInput1) == 13)

        let simulator2 = Simulator(numKnots: 10)
        assert(simulator2.simulate(input: testInput1) == 1)

        let simulator3 = Simulator(numKnots: 10)
        assert(simulator3.simulate(input: testInput2) == 36)
    }
}
