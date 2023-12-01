//
//  Solver_2022_24.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 24/12/2022.
//

import Foundation

class Solver_2022_24: Solver {
    class BlizzardSimulator {
        class Blizzard: Equatable, Hashable {
            let position: IntPoint
            let direction: Direction

            static func ==(lhs: Blizzard, rhs: Blizzard) -> Bool {
                lhs.position == rhs.position &&
                lhs.direction == rhs.direction
            }

            func hash(into hasher: inout Hasher) {
                hasher.combine(position)
                hasher.combine(direction)
            }

            init(position: IntPoint, direction: Direction) {
                self.position = position
                self.direction = direction
            }
        }

        let grid: StringGrid
        var blizzards: [Blizzard] = []
        var startPos: IntPoint = IntPoint(x: -1, y: -1)
        var endPos: IntPoint = IntPoint(x: -1, y: -1)
        let solver: Solver?

        init(strings: [String], solver: Solver? = nil) {
            let grid = StringGrid(stringArray: strings)

            for (y, row) in strings.enumerated() {
                for (x, char) in row.enumerated() {
                    if let direction = Direction(string: "\(char)") {
                        let position = IntPoint(x: x, y: y)
                        grid.setValue(at: position, to: ".")
                        blizzards.append(Blizzard(position: position, direction: direction))
                    }

                    if y == 0 && char == "." {
                        startPos = IntPoint(x: x, y: y)
                    } else if y == strings.count - 1 && char == "." {
                        endPos = IntPoint(x: x, y: y)
                    }
                }
            }

            self.grid = grid
            self.solver = solver
        }

        func getNextBlizzards(oldBlizzards: [Blizzard]) -> [Blizzard] {
            var newBlizzards: [Blizzard] = []

            oldBlizzards.forEach { blizzard in
                let newPosition = blizzard.position + blizzard.direction.movementVector
                if grid.getValue(at: newPosition) == "#" {
                    switch blizzard.direction {
                    case .north, .south:
                        newPosition.y = blizzard.direction == .south ? 1 : grid.height - 2
                    case .east, .west:
                        newPosition.x = blizzard.direction == .east ? 1 : grid.width - 2
                    }
                }
                newBlizzards.append(Blizzard(position: newPosition, direction: blizzard.direction))
            }

            return newBlizzards
        }

        func simulate() {
//            printMe()
            for _ in (0..<10) {
                self.blizzards = getNextBlizzards(oldBlizzards: self.blizzards)
//                printMe()
            }
        }

        struct State: Hashable {
            let time: Int
            let blizzardIndex: Int
            let expeditionPos: IntPoint
        }

        var blizzardSets: [Int: [Blizzard]] = [:]

        func findFewestMinutesToGoal() -> Int {
            var iterations = 0
            blizzardSets[iterations] = blizzards
            while true {
                let lastBlizzards = blizzardSets[iterations]!
                let nextBlizzards = getNextBlizzards(oldBlizzards: lastBlizzards)

                if nextBlizzards == blizzardSets[0] {
                    break
                } else {
                    iterations += 1
                    blizzardSets[iterations] = nextBlizzards
                }
            }

            let initialState = State(time: 513, blizzardIndex: 513 % blizzardSets.count, expeditionPos: startPos)
            var states: Set<State> = [initialState]
            var addedStates: Set<State> = [initialState]
            var best = 900

            let offsets: [IntPoint] = [IntPoint(x: 0, y: 0), IntPoint(north: 1), IntPoint(west: 1), IntPoint(south: 1), IntPoint(east: 1)]
            while let state = states.popFirst() {
                if state.expeditionPos == endPos {
                    let actualTime = state.time - 1
                    if actualTime < best {
                        print(actualTime)
                        best = actualTime
                    }
                }

                guard state.time + state.expeditionPos.manhattanDistance(to: endPos) < best else {
                    continue
                }

                let nextTime = state.time + 1
                let nextBlizzardIndex = (state.blizzardIndex + 1) % blizzardSets.count
                let currentBlizzardSet = blizzardSets[state.blizzardIndex]!

                offsets.forEach { offset in
                    let nextPos = state.expeditionPos + offset
                    guard grid.getValue(at: nextPos) == "." else {
                        return
                    }
                    guard !currentBlizzardSet.contains(where: { $0.position == nextPos } ) else {
                        return
                    }

                    guard !addedStates.contains(where: { state in
                        state.blizzardIndex == nextBlizzardIndex && state.expeditionPos == nextPos && state.time <= nextTime
                    }) else {
                        return
                    }

                    let nextState = State(time: nextTime, blizzardIndex: nextBlizzardIndex, expeditionPos: nextPos)
                    states = states.filter({ state in
                        state.blizzardIndex != nextState.blizzardIndex || state.expeditionPos != nextState.expeditionPos || state.time < nextState.time
                    })

                    states.insert(nextState)
                    addedStates.insert(nextState)
                }
            }





//            var maxIterations = 337
//
//            while best == Int.max {
////                maxIterations += 1
//                solver?.visualizeCurrentPart(text: "Current max = \(maxIterations)")
//
//                var states: PriorityQueue<State> = .init { lhs, rhs in
//                    let lhsDist = lhs.expeditionPos.manhattanDistance(to: self.endPos)
//                    let rhsDist = rhs.expeditionPos.manhattanDistance(to: self.endPos)
//
//                    if lhsDist == rhsDist {
//                        return lhs.time < rhs.time
//                    }
//
//                    return lhsDist < rhsDist
//                }
//
//                let initialState = State(time: 0, blizzards: blizzards, expeditionPos: startPos)
//                states.enqueue(initialState)
//
//                while let state = states.dequeue() {
//                    if state.expeditionPos == endPos {
//                        best = min(best, state.time)
//                        break
//                    }
//
//                    let nextTime = state.time + 1
//                    guard nextTime < maxIterations else {
//                        continue
//                    }
//
//                    guard state.time + state.expeditionPos.manhattanDistance(to: endPos) < (maxIterations + 2) else {
//                        continue
//                    }
//
//                    let nextBlizzards = getNextBlizzards(oldBlizzards: state.blizzards)
//
//                    offsets.forEach { offset in
//                        let nextPos = state.expeditionPos + offset
//                        guard grid.getValue(at: nextPos) == "." else {
//                            return
//                        }
//                        guard !nextBlizzards.contains(where: { $0.position == nextPos} ) else {
//                            return
//                        }
//                        let nextState = State(time: nextTime, blizzards: nextBlizzards, expeditionPos: nextPos)
//                        states.enqueue(nextState)
//                    }
//                }
//            }



            return best
        }

        func printMe(blizzards: Set<Blizzard>) {
            let text = grid.asText { position, string in
                let blizzardsInLocation = blizzards.filter { $0.position == position }
                let blizzardCount = blizzardsInLocation.count
                if blizzardCount > 0 {
                    return blizzardCount == 1 ? blizzardsInLocation.first!.direction.arrowString : "\(blizzardsInLocation.count)"
                } else {
                    return string
                }
            }

            print(text, "\n")
        }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let input = defaultInputFileString.loadAsStringArray()
        let blizzardSimulator = BlizzardSimulator(strings: input, solver: self)
        let fewest = blizzardSimulator.findFewestMinutesToGoal()
        return "\(fewest)"
    }
}
