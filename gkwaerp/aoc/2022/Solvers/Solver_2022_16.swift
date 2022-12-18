//
//  Solver_2022_16.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 16/12/2022.
//

import Foundation

class Solver_2022_16: Solver {
    class Valve: Equatable, Hashable {
        static func == (lhs: Valve, rhs: Valve) -> Bool {
            lhs.name == rhs.name
        }

        func hash(into hasher: inout Hasher) {
            hasher.combine(name)
        }

        let name: String
        var isOpen: Bool
        let flowRate: Int
        let connections: [String]

        var isInteresting: Bool {
            flowRate > 0 && !isOpen
        }

        init(string: String) {
            let split = string
                .replacingOccurrences(of: ";", with: "")
                .replacingOccurrences(of: ",", with: "")
                .components(separatedBy: " ")
            self.name = split[1]
            self.isOpen = false
            self.flowRate = Int(split[4].components(separatedBy: "=")[1])!
            self.connections = split[9...].map { $0 }
        }
    }

    class PressureSolver {
        /// Valve name --> Travel cost
        typealias PathDistanceMap = [String: Int]

        /// Valve name --> PathDistanceMap
        private let pathMap: [String: PathDistanceMap]
        private let valves: [Valve]

        /// `Set` because we don't really care about the order, just that the valves visited don't overlap between human and elephant
        private var validPaths: [Set<Valve>: Int] = [:]

        init(strings: [String]) {
            let valves = strings.map { Valve(string: $0) }

            let nodes: [AStarNode<String>] = valves.map { AStarNode(identifier: $0.name) }

            valves.forEach { valve in
                let valveNode = nodes.first(where: { $0.identifier ==  valve.name })!
                valve.connections.forEach { connection in
                    let connectionNode = nodes.first(where: { $0.identifier == connection })!
                    let edge = AStarEdge(to: connectionNode, cost: 1)
                    valveNode.edges.insert(edge)
                }
            }

            var pathMap: [String: PathDistanceMap] = [:]
            valves.forEach { valve in
                let valveNode = nodes.first(where: { $0.identifier == valve.name })!
                let result = IntAStar<String>.calculate(startNode: valveNode, mode: .findShortestPathsToAll)
                guard case let .gScores(pathDistanceMap) = result else {
                    fatalError("Huh?")
                }
                pathMap[valve.name] = pathDistanceMap
            }

            self.valves = valves
            self.pathMap = pathMap
        }

        private func dfs(currentTime: Int = 1,
                         currentPath: [Valve],
                         currentFlow: Int = 0,
                         currentReleasedPressure: Int = 0,
                         timeLimit: Int) -> Int {
            func savePath(path: [Valve], candidateBest: Int) {
                // Prevent false intersection of paths, where only common factor is start
                var actualPath = currentPath

                if actualPath.first!.name == "AA" {
                    _ = actualPath.removeFirst()
                }

                guard !actualPath.isEmpty else {
                    return
                }

                let pathSet = Set(actualPath)
                let actualBest = max(candidateBest, validPaths[pathSet] ?? 0)
                validPaths[pathSet] = actualBest
            }

            // Score we get if we were to idle rest of remaining time
            let remainingTime = timeLimit - currentTime + 1
            let idleScore = currentReleasedPressure + remainingTime * currentFlow

            var bestScore = idleScore
            savePath(path: currentPath, candidateBest: bestScore)

            let currentValve = currentPath.last!

            valves
                .filter { $0.isInteresting }
                .forEach { nextValve in
                    let durationForTravelAndAction = pathMap[currentValve.name]![nextValve.name]! + 1
                    let newTime = currentTime + durationForTravelAndAction
                    guard newTime <= timeLimit else {
                        return
                    }

                    let pressureReleasedDuringTravelAndAction = durationForTravelAndAction * currentFlow
                    let newReleasedPressure = currentReleasedPressure + pressureReleasedDuringTravelAndAction
                    let newFlow = currentFlow + nextValve.flowRate
                    let newPath = currentPath + [nextValve]

                    nextValve.isOpen.toggle()
                    let dfsResult = dfs(currentTime: newTime,
                                        currentPath: newPath,
                                        currentFlow: newFlow,
                                        currentReleasedPressure: newReleasedPressure,
                                        timeLimit: timeLimit)
                    nextValve.isOpen.toggle()

                    bestScore = max(bestScore, dfsResult)
                }

            return bestScore
        }

        func getHighestPressureRelease(withElephantAssistance: Bool) -> Int {
            // Reset to ensure no interference between p1 and p2
            validPaths = [:]

            let startValve = valves.first(where: { $0.name == "AA" })!
            let timeLimit = withElephantAssistance ? 26 : 30

            // This also populates validPaths, which we use for p2
            let dfsResult = dfs(currentPath: [startValve], timeLimit: timeLimit)

            if withElephantAssistance {
                var best = 0
                for humanPath in validPaths {
                    let humanSet = humanPath.key
                    for elephantPath in validPaths {
                        let elephantSet = elephantPath.key
                        let totalScore = humanPath.value + elephantPath.value
                        if best < totalScore, humanSet.isDisjoint(with: elephantSet) {
                            best = totalScore
                        }
                    }
                }
                return best
            } else {
                return dfsResult
            }
        }
    }

    private var pressureSolver: PressureSolver!
    override func didLoadFunction() {
        let input = defaultInputFileString.loadAsTextStringArray()
        pressureSolver = PressureSolver(strings: input)
    }

    override func solveFunction1() -> String {
        let pressure = pressureSolver.getHighestPressureRelease(withElephantAssistance: false)
        return "\(pressure)"
    }

    override func solveFunction2() -> String {
        let pressure = pressureSolver.getHighestPressureRelease(withElephantAssistance: true)
        return "\(pressure)"
    }
}

extension Solver_2022_16: TestableDay {
    func runTests() {
        let testInput = defaultTestInputString(suffix: "a").loadAsTextStringArray()
        let pressureSolver = PressureSolver(strings: testInput)

        let pressure1 = pressureSolver.getHighestPressureRelease(withElephantAssistance: false)
        assert(pressure1 == 1651)

        let pressure2 = pressureSolver.getHighestPressureRelease(withElephantAssistance: true)
        assert(pressure2 == 1707)
    }
}
