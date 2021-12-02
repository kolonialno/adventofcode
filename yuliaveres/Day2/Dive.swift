//
//  Dive.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 02/12/2021.
//

import Foundation

enum CommandKind: String {
    case forward
    case down
    case up
}

struct Command {
    let kind: CommandKind
    let steps: Int
}

struct Dive {

    func part1(input: String) throws -> Int {
        let commands = try DiveCommandsParser().parse(input: input)

        var horizontal: Int = 0
        var vertical: Int = 0

        commands.forEach { command in

            switch command.kind {
            case .up:
                vertical -= command.steps
            case .forward:
                horizontal += command.steps
            case .down:
                vertical += command.steps
            }
        }

        return horizontal * vertical
    }

    func part2(input: String) throws -> Int {
        let commands = try DiveCommandsParser().parse(input: input)

        var horizontal: Int = 0
        var vertical: Int = 0

        var aim: Int = 0

        commands.forEach { command in

            switch command.kind {
            case .up:
                aim -= command.steps
            case .forward:
                horizontal += command.steps
                vertical += aim * command.steps
            case .down:
                aim += command.steps
            }
        }

        return horizontal * vertical
    }
}

struct DiveCommandsParser {

    func parse(input: String) throws -> [Command] {

        let lines = input.components(separatedBy: "\n")

        return try lines.map { line -> Command in

            let components = line.components(separatedBy: " ")
            let commandString = components[0]
            let steps = Int(components[1]) ?? 0

            guard let kind = CommandKind(rawValue: commandString) else {
                throw ParseError.invalidCommandType(details: commandString)
            }

            return Command.init(kind: kind, steps: steps)
        }
    }
}

extension DiveCommandsParser {

    enum ParseError: LocalizedError {
        case invalidCommandType(details: String)

        var localizedDescription: String {
            let description: String

            switch self {
            case .invalidCommandType(let details):
                description = "Invalid command type: \(details)"
            }

            return description
        }
    }
}
