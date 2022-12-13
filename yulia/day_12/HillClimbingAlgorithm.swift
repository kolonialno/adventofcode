//
//  HillClimbingAlgorithm.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 12/12/2022.
//

import Foundation

struct HillClimbingAlgorithm {

    struct Position: Hashable {
        let x: Int
        let y: Int
    }

    func part1(_ input: String) -> Int {
        let lines = input.components(separatedBy: "\n").map { [Character]($0) }
        let myY = lines.firstIndex(where: { $0.contains("S") })!
        let myLine = lines[myY]
        let myPosition = Position(x: myLine.firstIndex(of: "S")!, y: myY)

        return leastSteps(from: myPosition, in: lines)
    }

    func part2(_ input: String) -> Int {
        let lines = input.components(separatedBy: "\n").map { [Character]($0) }
        let myY = lines.firstIndex(where: { $0.contains("S") })!
        let myLine = lines[myY]
        let myPosition = Position(x: myLine.firstIndex(of: "S")!, y: myY)

        var myPositions: [Position] = [myPosition]

        for (lineIdx, line) in lines.enumerated() {
            for (charIdx, char) in line.enumerated() {
                let position = Position(x: charIdx, y: lineIdx)
                if char == "a" {
                    myPositions.append(position)
                }
            }
        }

        let steps = myPositions.compactMap { myPos -> Int? in
            let least = leastSteps(from: myPos, in: lines)
            return least > 0 ? least : nil
        }

        return steps.min()!
    }

    private func leastSteps(from myPosition: Position, in lines: [[Character]]) -> Int {
        let eY = lines.firstIndex(where: { $0.contains("E") })!
        let eLine = lines[eY]
        let ePosition = Position(x: eLine.firstIndex(of: "E")!, y: eY)

        var visited = Set<Position>([myPosition])

        var stack: [(Position, Int)] = [(myPosition, 0)]
        while !stack.isEmpty {
            let position = stack.removeFirst()

            if position.0 == ePosition {
                // Goal!
                return position.1
            }

            if position.0.y > 0 {
                let thisChar = character(at: position.0, in: lines, my: myPosition, dest: ePosition)
                let topCharPos: Position = .init(x: position.0.x, y: position.0.y - 1)
                let topChar = character(at: topCharPos, in: lines, my: myPosition, dest: ePosition)
                appendIfNeeded(char: topChar, at: topCharPos, after: thisChar, thisCharPosition: position, visited: &visited, stack: &stack)
            }

            if position.0.x > 0 {
                let thisChar = character(at: position.0, in: lines, my: myPosition, dest: ePosition)
                let leftCharPos: Position = .init(x: position.0.x - 1, y: position.0.y)
                let leftChar = character(at: leftCharPos, in: lines, my: myPosition, dest: ePosition)
                appendIfNeeded(char: leftChar, at: leftCharPos, after: thisChar, thisCharPosition: position, visited: &visited, stack: &stack)
            }

            if position.0.y < lines.count - 1 {
                let thisChar = character(at: position.0, in: lines, my: myPosition, dest: ePosition)
                let bottomCharPos: Position = .init(x: position.0.x, y: position.0.y + 1)
                let bottomChar = character(at: bottomCharPos, in: lines, my: myPosition, dest: ePosition)
                appendIfNeeded(char: bottomChar, at: bottomCharPos, after: thisChar, thisCharPosition: position, visited: &visited, stack: &stack)
            }

            if position.0.x < lines[0].count - 1 {
                let thisChar = character(at: position.0, in: lines, my: myPosition, dest: ePosition)
                let rightCharPos: Position = .init(x: position.0.x + 1, y: position.0.y)
                let rightChar = character(at: rightCharPos, in: lines, my: myPosition, dest: ePosition)
                appendIfNeeded(char: rightChar, at: rightCharPos, after: thisChar, thisCharPosition: position, visited: &visited, stack: &stack)
            }
        }

        return 0
    }

    private func appendIfNeeded(char: Character, at position: Position, after thisChar: Character, thisCharPosition: (Position, Int), visited: inout Set<Position>, stack: inout [(Position, Int)]) {
        if !visited.contains(position) {
            if char < thisChar {
                stack.append((position, thisCharPosition.1 + 1))
                visited.insert(position)
            } else if char.asciiValue! - thisChar.asciiValue! <= 1 {
                stack.append((position, thisCharPosition.1 + 1))
                visited.insert(position)
            }
        }
    }

    private func character(at position: Position, in lines: [[Character]], my: Position, dest: Position) -> Character {
        let char: Character
        if position == my {
            char = "a"
        } else if position == dest {
            char = "z"
        } else {
            char = lines[position.y][position.x]
        }

        return char
    }
}
