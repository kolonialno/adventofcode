//
//  IntAStar.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class AStarEdge {
    var to: AStarNode
    var cost: Int

    init(to: AStarNode, cost: Int) {
        self.to = to
        self.cost = cost
    }
}

extension AStarEdge: Hashable, Equatable {
    static func == (lhs: AStarEdge, rhs: AStarEdge) -> Bool {
        lhs.to == rhs.to &&
        lhs.cost == rhs.cost
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(to)
        hasher.combine(cost)
    }
}

class AStarNode: Hashable, Equatable {
    let position: IntPoint
    var edges: Set<AStarEdge>

    init(position: IntPoint, edges: Set<AStarEdge> = []) {
        self.position = position
        self.edges = edges
    }

    static func == (lhs: AStarNode, rhs: AStarNode) -> Bool {
        lhs.position == rhs.position &&
        lhs.edges == rhs.edges
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(position)
    }
}

class IntAStar {
    typealias Heuristic = (AStarNode, AStarNode) -> Int

    struct Visit {
        let position: IntPoint
        let cost: Int
    }

    enum Result {
        struct Path {
            let positions: [IntPoint]
            let cost: Int
        }

        case path(Path)

        /// IntPoint --> cheapest path cost from `startNode`
        case gScores([IntPoint: Int])
    }

    enum Mode {
        case findShortestPathToGoal(AStarNode, Heuristic)
        case findShortestPathsToAll

        func getFScore(for node: AStarNode) -> Int {
            switch self {
            case .findShortestPathToGoal(let endNode, let heuristic):
                return heuristic(node, endNode)
            case .findShortestPathsToAll:
                return 0
            }
        }
    }

    static func calculate(startNode: AStarNode, mode: Mode) -> Result {
        /// IntPoint --> current cheapest path cost from `startNode`
        var gScores: [IntPoint: Int]  = [:]

        /// Node --> Current best guess at cheapest path cost from `startNode` to `endNode`, if path includes this node
        var fScores: [AStarNode: Int] = [:]

        /// IntPoint --> Previous visit (with lowest cost)
        var history: [IntPoint: Visit] = [:]

        gScores[startNode.position] = 0
        fScores[startNode] = mode.getFScore(for: startNode)

        var open: PriorityQueue<AStarNode> = .init(sort: { fScores[$0] ?? .max < fScores[$1] ?? .max } )
        open.enqueue(startNode)

        while let current = open.dequeue() {
            if case let .findShortestPathToGoal(endNode, _) = mode, current.position == endNode.position {
                var cost = 0
                var path: [IntPoint] = [current.position]
                while let before = history[path.first!] {
                    path.insert(before.position, at: 0)
                    cost += before.cost
                }

                return .path(Result.Path(positions: path, cost: cost))
            }


            current.edges.forEach { edge in
                let gScore = gScores[current.position]! + edge.cost
                if gScore < gScores[edge.to.position, default: .max] {
                    history[edge.to.position] = Visit(position: current.position, cost: edge.cost)
                    gScores[edge.to.position] = gScore
                    fScores[edge.to] = gScore + mode.getFScore(for: edge.to)

                    if !open.contains(node: edge.to) {
                        open.enqueue(edge.to)
                    }
                }
            }
        }

        guard case .findShortestPathsToAll = mode else {
            fatalError("Unable to find path to end node")
        }

        return .gScores(gScores)
    }
}

extension IntAStar {
    static func defaultHeuristic() -> Heuristic {
        return { (from, to) in
            return from.position.manhattanDistance(to: to.position)
        }
    }
}
