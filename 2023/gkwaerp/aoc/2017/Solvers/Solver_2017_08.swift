//
//  Solver_2017_08.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 15/11/2023.
//

import Foundation

final class Solver_2017_08: Solver {
    private struct RegisterResult {
        let highestEndValue: Int
        let highestSeenValue: Int
    }

    private enum Operation {
        case increment(value: Int)
        case decrement(value: Int)
    }

    private struct Instruction {
        let registerToWrite: String
        let operation: Operation
        let registerToRead: String
        let comparator: (Int, Int) -> Bool
        let comparatorValue: Int

        init(_ string: String) {
            let split = string.components(separatedBy: " ")
            self.registerToWrite = split[0]

            let value = Int(split[2])!
            self.operation = split[1] == "inc" ? .increment(value: value) : .decrement(value: value)

            self.registerToRead = split[4]

            switch split[5] {
            case ">": self.comparator = (>)
            case ">=": self.comparator = (>=)
            case "<": self.comparator = (<)
            case "<=": self.comparator = (<=)
            case "==": self.comparator = (==)
            case "!=": self.comparator = (!=)
            default: fatalError("Invalid comparator")
            }

            self.comparatorValue = Int(split[6])!
        }
    }

    private var input: [Instruction] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray().map { Instruction($0) }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let result = apply(input).highestEndValue
        return result
    }

    override func solveFunction2() -> CustomStringConvertible {
        let result = apply(input).highestSeenValue
        return result
    }

    private func apply(_ instructions: [Instruction]) -> RegisterResult {
        var registers: [String: Int] = [:]

        var highestSeenValue = 0

        instructions.forEach { instruction in
            let registerToWrite = instruction.registerToWrite
            let registerToRead = instruction.registerToRead
            let registerReadValue = registers[registerToRead, default: 0]

            if instruction.comparator(registerReadValue, instruction.comparatorValue) {
                switch instruction.operation {
                case .increment(let value):
                    registers[registerToWrite, default: 0] += value
                case .decrement(let value):
                    registers[registerToWrite, default: 0] -= value
                }

                highestSeenValue = max(highestSeenValue, registers[registerToWrite]!)
            }
        }

        let highestEndValue = registers.values.max()!
        return RegisterResult(highestEndValue: highestEndValue, highestSeenValue: highestSeenValue)
    }
}

extension Solver_2017_08: TestableDay {
    func runTests() {
        let input = """
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
"""
            .components(separatedBy: .newlines)

        let instructions = input.map { Instruction($0) }
        let result = apply(instructions)
        let expectedEnd = 1
        let expectedSeen = 10

        assert(result.highestEndValue == expectedEnd)
        assert(result.highestSeenValue == expectedSeen)
    }
}
