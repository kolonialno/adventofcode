//
//  Solver_2017_12.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 24/11/2023.
//

import Foundation

final class Solver_2017_12: Solver {
    private final class Pipe: Equatable, Hashable {
        let id: Int
        let connectionIds: [Int]
        var connections: Set<Pipe>

        init(_ string: String) {
            let split = string.components(separatedBy: " <-> ")
            id = split[0].intValue!
            let connectionIds = split[1]
                .components(separatedBy: ", ")
                .map { $0.intValue! } + [id]
            self.connectionIds = connectionIds
            self.connections = []
        }

        func hash(into hasher: inout Hasher) {
            hasher.combine(id)
        }

        static func == (lhs: Pipe, rhs: Pipe) -> Bool {
            lhs.id == rhs.id
        }
    }

    private struct PipeManager {
        let pipes: [Pipe]

        init(pipes: [Pipe]) {
            self.pipes = pipes
            pipes.forEach { pipe in
                pipe.connectionIds.forEach { connectionId in
                    let connectedPipe = pipes.first(where: { $0.id == connectionId })!
                    pipe.connections.insert(connectedPipe)
                }
            }
        }

        func getNumPipes(connectedTo id: Int) -> Int {
            var visited: Set<Pipe> = []
            var toVisit: [Pipe] = []

            let startPipe = pipes.first(where: { $0.id == id })!
            toVisit = Array(startPipe.connections)

            while let pipe = toVisit.popLast() {
                pipe.connections.forEach { connection in
                    let insertion = visited.insert(connection)
                    guard insertion.inserted else { return }
                    toVisit.append(connection)
                }
            }

            return visited.count
        }

        func getNumGroups() -> Int {
            var numGroups = 0
            var unseenPipes: Set<Pipe> = Set(pipes)

            while let rootPipe = unseenPipes.first {
                numGroups += 1

                var toVisit: [Pipe] = [rootPipe]
                while let pipe = toVisit.popLast() {
                    guard unseenPipes.contains(pipe) else { continue }
                    unseenPipes.remove(pipe)
                    pipe.connections.forEach { connection in
                        toVisit.append(connection)
                    }
                }
            }

            return numGroups
        }
    }

    private var pipes: [Pipe] = []

    override func didLoadFunction() {
        pipes = defaultInputFileString.loadAsStringArray().map { Pipe($0) }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let pipeManager = PipeManager(pipes: pipes)
        return pipeManager.getNumPipes(connectedTo: 0)
    }

    override func solveFunction2() -> CustomStringConvertible {
        let pipeManager = PipeManager(pipes: pipes)
        return pipeManager.getNumGroups()
    }
}

extension Solver_2017_12: TestableDay {
    func runTests() {
        let input = """
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
"""
            .components(separatedBy: .newlines)

        let pipes = input.map { Pipe($0) }
        let pipeManager = PipeManager(pipes: pipes)
        let result = pipeManager.getNumPipes(connectedTo: 0)
        let expected = 6
        assert(result == expected)

        let numGroups = pipeManager.getNumGroups()
        let expectedGroups = 2
        assert(numGroups == expectedGroups)
    }
}
