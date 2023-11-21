//
//  IntAStar.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class AStarEdge<T> where T: Hashable, T:Equatable {
    var to: AStarNode<T>
    var cost: Int

    init(to: AStarNode<T>, cost: Int) {
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

class AStarNode<T>: Hashable, Equatable where T: Hashable, T: Equatable {
    let identifier: T
    var edges: Set<AStarEdge<T>>

    init(identifier: T, edges: Set<AStarEdge<T>> = []) {
        self.identifier = identifier
        self.edges = edges
    }

    static func == (lhs: AStarNode, rhs: AStarNode) -> Bool {
        lhs.identifier == rhs.identifier &&
        lhs.edges == rhs.edges
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(identifier)
    }
}

class IntAStar<T> where T: Hashable, T: Equatable {
    typealias Heuristic = (AStarNode<T>, AStarNode<T>) -> Int

    struct Visit {
        let position: T
        let cost: Int
    }

    enum Result {
        struct Path {
            let positions: [T]
            let cost: Int
        }

        case path(Path)

        /// IntPoint --> cheapest path cost from `startNode`
        case gScores([T: Int])
    }

    enum Mode {
        case findShortestPathToGoal(AStarNode<T>, Heuristic)
        case findShortestPathsToAll

        func getFScore(for node: AStarNode<T>) -> Int {
            switch self {
            case .findShortestPathToGoal(let endNode, let heuristic):
                return heuristic(node, endNode)
            case .findShortestPathsToAll:
                return 0
            }
        }
    }

    static func calculate(startNode: AStarNode<T>, mode: Mode) -> Result {
        /// IntPoint --> current cheapest path cost from `startNode`
        var gScores: [T: Int]  = [:]

        /// Node --> Current best guess at cheapest path cost from `startNode` to `endNode`, if path includes this node
        var fScores: [AStarNode<T>: Int] = [:]

        /// IntPoint --> Previous visit (with lowest cost)
        var history: [T: Visit] = [:]

        gScores[startNode.identifier] = 0
        fScores[startNode] = mode.getFScore(for: startNode)

        var open: PriorityQueue<AStarNode<T>> = .init(sort: { fScores[$0] ?? .max < fScores[$1] ?? .max } )
        open.enqueue(startNode)

        while let current = open.dequeue() {
            if case let .findShortestPathToGoal(endNode, _) = mode, current.identifier == endNode.identifier {
                var cost = 0
                var path: [T] = [current.identifier]
                while let before = history[path.first!] {
                    path.insert(before.position, at: 0)
                    cost += before.cost
                }

                return .path(Result.Path(positions: path, cost: cost))
            }


            current.edges.forEach { edge in
                let gScore = gScores[current.identifier]! + edge.cost
                if gScore < gScores[edge.to.identifier, default: .max] {
                    history[edge.to.identifier] = Visit(position: current.identifier, cost: edge.cost)
                    gScores[edge.to.identifier] = gScore
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
    static func defaultHeuristic() -> Heuristic where T: IntPoint {
        return { (from, to) in
            return from.identifier.manhattanDistance(to: to.identifier)
        }
    }
}
