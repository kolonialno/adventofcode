//
//  Solver_2022_22.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 22/12/2022.
//

import Foundation

class Solver_2022_22: Solver {
    enum Instruction {
        case move(numSteps: Int)
        case turn(left: Bool)

        init(string: String) {
            if let numSteps = Int(string) {
                self = .move(numSteps: numSteps)
            } else if string == "L" {
                self = .turn(left: true)
            } else if string == "R" {
                self = .turn(left: false)
            } else {
                fatalError("Invalid string \(string)")
            }
        }
    }

    class MonkeyMap {
        let areaMap: StringGrid
        let instructions: [Instruction]
        var lastFacingMap: [IntPoint: Direction]

        let isBox: Bool

        var currPos: IntPoint
        var currDirection: Direction

        init(string: String, isBox: Bool) {
            let mainSplit = string.components(separatedBy: "\n\n")
            let areaMapStrings = mainSplit[0].components(separatedBy: .newlines)
            let instructionsStrings = mainSplit[1]
                .replacingOccurrences(of: "R", with: " R ")
                .replacingOccurrences(of: "L", with: " L ")
                .replacingOccurrences(of: "  ", with: " ")
                .replacingOccurrences(of: "\n", with: "")
                .components(separatedBy: " ")
                .filter { !$0.isEmpty }

            let width = areaMapStrings.map{ $0.count }.max()!
            let height = areaMapStrings.count
            let size = IntPoint(x: width, y: height)
            let areaMap = StringGrid(size: size, fillWith: "⬛️")

            for (y, column) in areaMapStrings.enumerated() {
                for (x, row) in column.enumerated() {
                    let pos = IntPoint(x: x, y: y)
                    if row == "#" {
                        areaMap.setValue(at: pos, to: "🪨")
                    } else if row == "." {
                        areaMap.setValue(at: pos, to: "🟧")
                    }
                }
            }

            self.areaMap = areaMap
            self.instructions = instructionsStrings.map { Instruction(string: $0) }
            self.isBox = isBox
            self.lastFacingMap = [:]
            self.currDirection = .east
            self.currPos = IntPoint(x: -1, y: -1)
            for x in 0..<areaMap.width {
                if areaMap.getValue(at: IntPoint(x: x, y: 0)) == "🟧" {
                    currPos = IntPoint(x: x, y: 0)
                }
            }
            assert(self.currPos != IntPoint(x: -1, y: -1))
        }

        func findPassword() -> Int {
            for instruction in instructions {
                perform(instruction)
            }


            let directionValue: Int
            switch currDirection {
            case .north:
                directionValue = 3
            case .south:
                directionValue = 1
            case .west:
                directionValue = 2
            case .east:
                directionValue = 0
            }
            return (currPos.y + 1) * 1000 + (currPos.x + 1) * 4 + directionValue
        }

        enum WalkResult {
            case rock
            case path
            case void
        }

        private func getWalkResult(for candidatePos: IntPoint) -> WalkResult {
            if areaMap.getValue(at: candidatePos) == "🟧" {
                return .path
            } else if areaMap.getValue(at: candidatePos) == "🪨" {
                return .rock
            } else {
                return .void
            }
        }

        private func findLoopPointAndDirection() -> (IntPoint, Direction)? {
            return isBox ? findLoopPointAndDirectionInBox() : findLoopPointAndDirectionNormal()
        }

        private func findLoopPointAndDirectionInBox() -> (IntPoint, Direction)? {
            switch currDirection {
            case .north:
                if (100..<150).contains(currPos.x) { // A -> F
                    let candidatePos = IntPoint(x: 0 + currPos.x - 100, y: 199)
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .north) : nil
                } else if (50..<100).contains(currPos.x) { // B -> F
                    let candidatePos = IntPoint(x: 0, y: 150 + currPos.x - 50)
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .east) : nil
                } else if (0..<50).contains(currPos.x) { // E -> C
                    let candidatePos = IntPoint(x: 50, y: 50 + currPos.x - 0)
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .east) : nil
                }
            case .south:
                if (100..<150).contains(currPos.x) { // A -> C
                    let candidatePos = IntPoint(x: 99, y: 50 + currPos.x - 100)
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .west) : nil
                } else if (50..<100).contains(currPos.x) { // D -> F
                    let candidatePos = IntPoint(x: 49, y: 150 + currPos.x - 50)
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .west) : nil
                } else if (0..<50).contains(currPos.x) { // F -> A
                    let candidatePos = IntPoint(x: 100 + currPos.x - 0, y: 0)
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .south) : nil
                }
            case .west:
                if (0..<50).contains(currPos.y) { // B -> E
                    let candidatePos = IntPoint(x: 0, y: 149 - (currPos.y - 0))
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .east) : nil
                } else if (50..<100).contains(currPos.y) { // C -> E
                    let candidatePos = IntPoint(x: 0 + currPos.y - 50, y: 100)
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .south) : nil
                } else if (100..<150).contains(currPos.y) { // E -> B
                    let candidatePos = IntPoint(x: 50, y: 49 - (currPos.y - 100))
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .east) : nil
                } else if (150..<200).contains(currPos.y) { // F -> B
                    let candidatePos = IntPoint(x: 50 + currPos.y - 150, y: 0)
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .south) : nil
                }
            case .east:
                if (0..<50).contains(currPos.y) { // A -> D
                    let candidatePos = IntPoint(x: 99, y: 149 - (currPos.y - 0))
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .west) : nil
                } else if (50..<100).contains(currPos.y) { // C -> A
                    let candidatePos = IntPoint(x: 100 + currPos.y - 50, y: 49)
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .north) : nil
                } else if (100..<150).contains(currPos.y) { // D -> A
                    let candidatePos = IntPoint(x: 149, y: 49 - (currPos.y - 100))
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .west) : nil
                } else if (150..<200).contains(currPos.y) { // F -> D
                    let candidatePos = IntPoint(x: 50 + currPos.y - 150, y: 149)
                    return getWalkResult(for: candidatePos) == .path ? (candidatePos, .north) : nil
                }
            }

            fatalError()
        }

        private func findLoopPointAndDirectionNormal() -> (IntPoint, Direction)? {
            switch currDirection {
            case .north:
                for y in (0..<areaMap.height).reversed() {
                    let candidatePos = IntPoint(x: currPos.x, y: y)
                    switch getWalkResult(for: candidatePos) {
                    case .path:
                        return (candidatePos, currDirection)
                    case .rock:
                        return nil
                    case .void:
                        break
                    }
                }
            case .south:
                for y in (0..<areaMap.height) {
                    let candidatePos = IntPoint(x: currPos.x, y: y)
                    switch getWalkResult(for: candidatePos) {
                    case .path:
                        return (candidatePos, currDirection)
                    case .rock:
                        return nil
                    case .void:
                        break
                    }
                }
            case .west:
                for x in (0..<areaMap.width).reversed() {
                    let candidatePos = IntPoint(x: x, y: currPos.y)
                    switch getWalkResult(for: candidatePos) {
                    case .path:
                        return (candidatePos, currDirection)
                    case .rock:
                        return nil
                    case .void:
                        break
                    }
                }
            case .east:
                for x in (0..<areaMap.width) {
                    let candidatePos = IntPoint(x: x, y: currPos.y)
                    switch getWalkResult(for: candidatePos) {
                    case .path:
                        return (candidatePos, currDirection)
                    case .rock:
                        return nil
                    case .void:
                        break
                    }
                }
            }

            return nil
        }

        private func move(numSteps: Int) {
            for _ in (0..<numSteps) {
                let nextPos = currPos + currDirection.movementVector
                switch getWalkResult(for: nextPos) {
                case .path:
                    currPos = nextPos
                case .rock:
                    break
                case .void:
                    if let loopResult = findLoopPointAndDirection() {
                        currPos = loopResult.0
                        currDirection = loopResult.1
                    }
                }
                lastFacingMap[currPos] = currDirection
            }
        }

        private func perform(_ instruction: Instruction) {
            switch instruction {
            case .turn(let left):
                currDirection.turn(left: left)
                lastFacingMap[currPos] = currDirection
            case .move(let numSteps):
                move(numSteps: numSteps)
            }
        }
    }

    private var input: String = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString(trimming: false)
    }

    override func solveFunction1() -> String {
        let monkeyMap = MonkeyMap(string: input, isBox: false)
        let password = monkeyMap.findPassword()
        return "\(password)"
    }

    override func solveFunction2() -> String {
        let monkeyMap = MonkeyMap(string: input, isBox: true)
        let password = monkeyMap.findPassword()
        return "\(password)"
    }
}


extension Solver_2022_22: TestableDay {
    func runTests() {
        let testInput = defaultTestInputString(suffix: "a").loadAsTextString(trimming: false)
        let monkeyMap1 = MonkeyMap(string: testInput, isBox: false)
        let password1 = monkeyMap1.findPassword()
        assert(password1 == 6032)
    }
}
