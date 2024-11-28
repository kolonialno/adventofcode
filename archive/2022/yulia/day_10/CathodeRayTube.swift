//
//  CathodeRayTube.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 11/12/2022.
//

import Foundation

struct CathodeRayTube {

    func part1(_ input: String) -> Int {
        var cycle: Int = 0
        var x: Int = 1

        let instructions = input.components(separatedBy: "\n")
        var xToAdd: Int?
        var signalStrength: [Int] = []

        for instructionStr in instructions {
            print(instructionStr)
            if let toAdd = xToAdd {
                x += toAdd
                xToAdd = nil
            }

            let components = instructionStr.components(separatedBy: " ")
            let instruction = components[0]

            let numOfCycles: Int
            switch instruction {
            case "addx":
                numOfCycles = 2
            case "noop":
                numOfCycles = 1
            default:
                fatalError("Unexpected instruction")
            }

            for instructionCycle in Array(1...numOfCycles) {
                cycle += 1
                if instruction == "addx" && instructionCycle == 2 {
                    xToAdd = Int(components[1])!
                }

                if (cycle - 20) % 40 == 0 {
                    signalStrength.append(x * cycle)
                }
            }
        }

        if let toAdd = xToAdd {
            x += toAdd
            xToAdd = nil
        }

        return signalStrength.reduce(0, +)
    }

    func part2(_ input: String) -> Int {
        var cycle: Int = 0
        var x: Int = 1

        let instructions = input.components(separatedBy: "\n")
        var xToAdd: Int?

        var lines: [String] = []

        for instructionStr in instructions {
            print(instructionStr)
            if let toAdd = xToAdd {
                x += toAdd
                xToAdd = nil
            }

            let components = instructionStr.components(separatedBy: " ")
            let instruction = components[0]

            let numOfCycles: Int
            switch instruction {
            case "addx":
                numOfCycles = 2
            case "noop":
                numOfCycles = 1
            default:
                fatalError("Unexpected instruction")
            }

            for instructionCycle in Array(1...numOfCycles) {
                cycle += 1
                if instruction == "addx" && instructionCycle == 2 {
                    xToAdd = Int(components[1])!
                }

                let char: String
                let cycleIdx = cycle % 40
                if x == cycleIdx || x+1 == cycleIdx || x+2 == cycleIdx {
                    char = "#"
                } else {
                    char = "."
                }

                if var lastLine = lines.last, lastLine.count < 40 {
                    lastLine.append(char)
                    lines = lines.dropLast(1)
                    lines.append(lastLine)
                } else {
                    lines.append(char)
                }
            }
        }

        if let toAdd = xToAdd {
            x += toAdd
            xToAdd = nil
        }

        for line in lines {
            print(line)
        }

        return 0
    }
}
