//
//  Solver_2022_12.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 12/12/2022.
//

import Foundation

class Solver_2022_12: Solver {
    private var aStar: IntAStar<IntPoint>!

    private func getElevation(for string: String) -> Int {
        if string == "S" {
            return 1
        } else if string == "E" {
            return 26
        } else {
            return Int(string.first!.asciiValue!) - Int("a".first!.asciiValue!) + 1
        }
    }

    private func getShortestDistance(in grid: StringGrid, findingScenicRoute: Bool) -> Int {
        let nodes = grid.createAStarNodes(walkableBlock: StringGrid.defaultWalkableBlock()) { from, to in
            let elevationFrom = getElevation(for: grid.getValue(at: from)!)
            let elevationTo = getElevation(for: grid.getValue(at: to)!)

            let isWalkable = elevationTo <= elevationFrom + 1
            return isWalkable ? 1 : .max
        }

        let startNode = nodes[grid.firstPosition(matching: { $0 == "S" })!]!
        let endNode = nodes[grid.firstPosition(matching: { $0 == "E" })!]!

        if findingScenicRoute {
            // Create free wormholes to all other "a" nodes, simulating that we start in all these places simultaneously
            grid.positions(matching: { $0 == "a" })
                .map { nodes[$0]! }
                .forEach { startNode.edges.insert(AStarEdge(to: $0, cost: 0)) }
        }

        let result = IntAStar.calculate(startNode: startNode,
                                        mode: .findShortestPathToGoal(endNode, IntAStar.defaultHeuristic()))

        guard case let .path(path) = result else {
            fatalError("A* unable to find valid path")
        }

        return path.cost
    }


    private var grid: StringGrid!

    override func didLoadFunction() {
        grid = defaultInputFileString.loadAsStringGrid()
    }

    override func solveFunction1() -> String {
        let distance = getShortestDistance(in: grid, findingScenicRoute: false)
        return "\(distance)"
    }

    override func solveFunction2() -> String {
        let distance = getShortestDistance(in: grid, findingScenicRoute: true)
        return "\(distance)"
    }
}

extension Solver_2022_12: TestableDay {
    func runTests() {
        let testGrid = defaultTestInputString(suffix: "a").loadAsStringGrid()
        let distance = getShortestDistance(in: testGrid, findingScenicRoute: false)
        assert(distance == 31)

        let distance2 = getShortestDistance(in: testGrid, findingScenicRoute: true)
        assert(distance2 == 29)
    }
}
