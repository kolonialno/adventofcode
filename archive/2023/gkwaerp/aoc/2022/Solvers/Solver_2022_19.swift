//
//  Solver_2022_19.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 19/12/2022.
//

import Foundation

class Solver_2022_19: Solver {
    enum ResourceType: Int, CaseIterable {
        case ore
        case clay
        case obsidian
        case geode
    }

    class State: Equatable, Hashable {
        let inventory: [Int]
        let robotInventory: [Int]
        let currentTime: Int

        init(inventory: [Int], robotInventory: [Int], currentTime: Int) {
            self.inventory = inventory
            self.robotInventory = robotInventory
            self.currentTime = currentTime
        }

        static func ==(lhs: State, rhs: State) -> Bool {
            lhs.inventory == rhs.inventory &&
            lhs.robotInventory == rhs.robotInventory &&
            lhs.currentTime == rhs.currentTime
        }

        func hash(into hasher: inout Hasher) {
            hasher.combine(inventory)
            hasher.combine(robotInventory)
            hasher.combine(currentTime)
        }

        func canAfford(cost: [Int]) -> Bool {
            ResourceType.allCases.allSatisfy { resourceType in
                let index = resourceType.rawValue
                return inventory[index] >= cost[index]
            }
        }

        func copying(deltaResources: [Int]? = nil, deltaRobots: [Int]? = nil, deltaTime: Int? = 1) -> State {
            var newInventory = inventory
            var newRobotInventory = robotInventory
            var newTime = currentTime

            if let deltaResources {
                ResourceType.allCases.forEach { resourceType in
                    let index = resourceType.rawValue
                    newInventory[index] += deltaResources[index]
                }
            }
            if let deltaRobots {
                ResourceType.allCases.forEach { resourceType in
                    let index = resourceType.rawValue
                    newRobotInventory[index] += deltaRobots[index]
                }
            }
            if let deltaTime {
                newTime += deltaTime
            }

            return State(inventory: newInventory, robotInventory: newRobotInventory, currentTime: newTime)
        }
    }

    class Blueprint {
        let id: Int

        /// Robot --> resources needed
        let costs: [ResourceType: [Int]]
        var geodes: Int

        var qualityLevel: Int {
            return geodes * id
        }

        init(string: String) {
            let ints = string.replacingOccurrences(of: ":", with: "")
                .components(separatedBy: " ")
                .compactMap { Int($0) }

            assert(ints.count == 7)

            self.id = ints[0]
            self.costs = [.ore: [ints[1], 0, 0, 0],
                          .clay: [ints[2], 0, 0, 0],
                          .obsidian: [ints[3], ints[4], 0, 0],
                          .geode: [ints[5], 0, ints[6], 0]]
            self.geodes = 0
        }

        func randomSolve(startState: State, timeLimit: Int, iterations: Int) {
            var best = 0

            for _ in 0..<iterations {
                var state = startState.copying(deltaTime: nil)
                for _ in 0...timeLimit {
                    guard state.currentTime <= timeLimit else {
                        break
                    }
                    if state.canAfford(cost: costs[.geode]!) && Int.random(in: 0...99) < 75 {
                        let deltaResources = (0..<4).map { -costs[.geode]![$0] + state.robotInventory[$0] }
                        state = state.copying(deltaResources: deltaResources, deltaRobots: [0, 0, 0, 1])
                    } else if state.canAfford(cost: costs[.obsidian]!) && Int.random(in: 0...99) < 75 {
                        let deltaResources = (0..<4).map { -costs[.obsidian]![$0] + state.robotInventory[$0] }
                        state = state.copying(deltaResources: deltaResources, deltaRobots: [0, 0, 1, 0])
                    } else if state.canAfford(cost: costs[.clay]!) && Bool.random() {
                        let deltaResources = (0..<4).map { -costs[.clay]![$0] + state.robotInventory[$0] }
                        state = state.copying(deltaResources: deltaResources, deltaRobots: [0, 1, 0, 0])
                    } else if state.canAfford(cost: costs[.ore]!) && Bool.random() {
                        let deltaResources = (0..<4).map { -costs[.ore]![$0] + state.robotInventory[$0] }
                        state = state.copying(deltaResources: deltaResources, deltaRobots: [1, 0, 0, 0])
                    } else {
                        state = state.copying(deltaResources: state.robotInventory, deltaTime: 1)
                    }
                }
                best = max(best, state.inventory[3])
            }

            geodes = best
        }
    }

    class BlueprintManager {
        let solver: Solver?
        let blueprints: [Blueprint]
        init(strings: [String], solver: Solver? = nil) {
            self.blueprints = strings.map { Blueprint(string: $0) }
            self.solver = solver
        }

        func getStartState() -> State {
            State(inventory: [0, 0, 0, 0],
                  robotInventory: [1, 0, 0, 0],
                  currentTime: 1)
        }

        func run(part1: Bool) -> Int {
            let blueprintsToRun = part1 ? blueprints : Array(blueprints.prefix(3))
            let timeLimit = part1 ? 24 : 32
            let iterations = part1 ? 200_000 : 2_000_000
            blueprintsToRun.enumerated().forEach { (index, blueprint) in
                let text = "\(index) / \(blueprintsToRun.count)  (\((index * 100) / blueprintsToRun.count) %)"
                solver?.visualizeCurrentPart(text: text)
                blueprint.randomSolve(startState: getStartState(), timeLimit: timeLimit, iterations: iterations)
            }
            solver?.visualizeCurrentPart(text: "\(blueprintsToRun.count) / \(blueprintsToRun.count) (100 %)")

            if part1 {
                return blueprintsToRun.map { $0.qualityLevel }.reduce(0, +)
            } else {
                return blueprintsToRun.map { $0.geodes }.reduce(1, *)
            }
        }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let blueprintManager = BlueprintManager(strings: defaultInputFileString.loadAsStringArray(), solver: self)
        let result = blueprintManager.run(part1: true)
        return "\(result)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let blueprintManager = BlueprintManager(strings: defaultInputFileString.loadAsStringArray(), solver: self)
        let result = blueprintManager.run(part1: false)
        return "\(result)"
    }
}
