//
//  Solver_2017_13.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 26/11/2023.
//

import Foundation

final class Solver_2017_13: Solver {
    private final class Firewall {

        private final class Layer {
            let range: Int
            let cycleLength: Int

            init(range: Int) {
                self.range = range
                self.cycleLength = (range - 1) * 2
            }

            func detects(at depth: Int, with delay: Int) -> Bool {
                (depth + delay).isMultiple(of: cycleLength)
            }
        }

        /// Depth -> Layer
        private typealias Layers = [Int: Layer]
        private let layers: Layers
        private let maxDepth: Int

        init(_ strings: [String]) {
            var maxDepth = 0
            var layers: Layers = [:]
            strings.forEach { string in
                let split = string.components(separatedBy: ": ")
                let depth = split[0].intValue!
                let layer = Layer(range: split[1].intValue!)
                layers[depth] = layer
                maxDepth = max(maxDepth, depth)
            }

            self.layers = layers
            self.maxDepth = maxDepth
        }

        func getSeverity() -> Int {
            var severity = 0
            var packetPosition = 0

            while packetPosition <= maxDepth {
                let isDetected = layers[packetPosition]?.detects(at: packetPosition, with: 0) ?? false
                if isDetected {
                    severity += layers[packetPosition]!.range * packetPosition
                }
                packetPosition += 1
            }

            return severity
        }

        func getPicoSecondsToDelay() -> Int {
            var delay = 0

            var done = false
            while !done {
                var isDetected = false
                for (depth, layer) in layers {
                    if layer.detects(at: depth, with: delay) {
                        isDetected = true
                        break
                    }
                }

                if isDetected {
                    delay += 1
                } else {
                    done = true
                }
            }

            return delay
        }
    }

    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let firewall = Firewall(input)
        return firewall.getSeverity()
    }

    override func solveFunction2() -> CustomStringConvertible {
        let firewall = Firewall(input)
        return firewall.getPicoSecondsToDelay()
    }
}

extension Solver_2017_13: TestableDay {
    func runTests() {
        let input = """
0: 3
1: 2
4: 4
6: 4
"""
            .components(separatedBy: .newlines)

        let firewall = Firewall(input)
        let severity = firewall.getSeverity()
        let expectedSeverity = 24
        assert(severity == expectedSeverity)

        let firewall2 = Firewall(input)
        let delay = firewall2.getPicoSecondsToDelay()
        let expectedDelay = 10
        assert(delay == expectedDelay)
    }
}
