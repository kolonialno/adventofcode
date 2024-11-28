//
//  RopeBridge.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 09/12/2022.
//

import Foundation

struct RopeBridge {

    struct Position: Hashable {
        var x: Int
        var y: Int
    }

    func part(_ input: String, part1: Bool) -> Int {
        let lines = input.components(separatedBy: "\n")

        var head = Position(x: 0, y: 0)
        var knots = Array(repeating: head, count: part1 ? 1 : 9)
        var visited = Set<Position>([head])

        for line in lines {

            let comp = line.components(separatedBy: " ")
            let move = comp[0]
            let steps = Int(comp[1])!

            for _ in Array(1...steps) {
                var newHead: Position
                switch move {
                case "R":
                    newHead = Position(x: head.x + 1, y: head.y)
                case "U":
                    newHead = Position(x: head.x, y: head.y - 1)
                case "L":
                    newHead = Position(x: head.x - 1, y: head.y)
                case "D":
                    newHead = Position(x: head.x, y: head.y + 1)
                default:
                    fatalError("Unknown move")
                }

                head = newHead

                for i in 0..<knots.count {
                    var knot = knots[i]

                    let diffX = newHead.x - knot.x
                    let diffY = newHead.y - knot.y

                    if abs(diffX) > 1 || abs(diffY) > 1 {
                        knot.x += diffX.signum()
                        knot.y += diffY.signum()

                        knots[i] = knot
                    } else {
                        break
                    }

                    newHead = knot
                }

                visited.insert(knots.last!)
            }
        }

        return visited.count
    }
}
