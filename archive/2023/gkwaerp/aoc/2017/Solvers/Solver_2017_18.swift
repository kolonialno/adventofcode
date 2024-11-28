//
//  Solver_2017_18.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 27/11/2023.
//

import Foundation

final class Solver_2017_18: Solver {
    enum Instruction {
        enum Operand {
            case register(r: String)
            case value(v: Int)

            init(_ string: String) {
                if let intValue = string.intValue {
                    self = .value(v: intValue)
                } else {
                    self = .register(r: string)
                }
            }
        }

        case snd(operand: Operand)
        case set(register: String, operand: Operand)
        case add(register: String, operand: Operand)
        case mul(register: String, operand: Operand)
        case mod(register: String, operand: Operand)
        case rcv(operand: Operand)
        case jgz(operand: Operand, offset: Operand)

        init(_ string: String) {
            let split = string.components(separatedBy: " ")
            switch split[0] {
            case "snd":
                self = .snd(operand: .init(split[1]))
            case "set":
                self = .set(register: split[1], operand: .init(split[2]))
            case "add":
                self = .add(register: split[1], operand: .init(split[2]))
            case "mul":
                self = .mul(register: split[1], operand: .init(split[2]))
            case "mod":
                self = .mod(register: split[1], operand: .init(split[2]))
            case "rcv":
                self = .rcv(operand: .init(split[1]))
            case "jgz":
                self = .jgz(operand: .init(split[1]), offset: .init(split[2]))
            default: fatalError("Invalid instruction")
            }
        }
    }

    final class Machine {
        var registers: [String: Int]
        var instructions: [Instruction]
        var instructionPointer: Int
        var lastFrequencyPlayed: Int?

        init(instructions: [Instruction]) {
            self.registers = [:]
            self.instructions = instructions
            self.instructionPointer = 0
            self.lastFrequencyPlayed = 0
        }

        func runUntilFirstRecoveredFrequency() -> Int? {
            let numInstructions = instructions.count
            while (0..<numInstructions).contains(instructionPointer) {
                let recoveredFrequency = perform(instruction: instructions[instructionPointer])
                if let recoveredFrequency {
                    return recoveredFrequency
                }
            }
            return nil
        }

        /// Returns recovered frequency if applicable, `nil` otherwise.
        func perform(instruction: Instruction) -> Int? {
            var didJump = false
            defer { self.instructionPointer += didJump ? 0 : 1 }
            switch instruction {
            case .snd(let operand):
                lastFrequencyPlayed = getValue(for: operand)
            case .set(let register, let operand):
                registers[register] = getValue(for: operand)
            case .add(let register, let operand):
                registers[register, default: 0] += getValue(for: operand)
            case .mul(let register, let operand):
                registers[register, default: 0] *= getValue(for: operand)
            case .mod(let register, let operand):
                registers[register, default: 0] %= getValue(for: operand)
            case .rcv(let operand):
                if getValue(for: operand) != 0 {
                    return lastFrequencyPlayed
                }
            case .jgz(let operand, let offset):
                if getValue(for: operand) > 0 {
                    self.instructionPointer += getValue(for: offset)
                    didJump = true
                }
            }
            return nil
        }

        private func getValue(for operand: Instruction.Operand) -> Int {
            switch operand {
            case .register(let r):
                return registers[r, default: 0]
            case .value(let v):
                return v
            }
        }
    }

    final class DuetMachine {
        final class Machine {
            var registers: [String: Int]
            var instructions: [Instruction]
            var instructionPointer: Int
            var queue: [Int]
            var sendClosure: ((Int) -> Void)?

            init(instructions: [Instruction], programID: Int) {
                self.registers = ["p": programID]
                self.instructions = instructions
                self.instructionPointer = 0
                self.queue = []
                self.sendClosure = nil
            }

            /// Returns whether operation was completed
            func tick() -> Bool {
                guard (0..<instructions.count).contains(instructionPointer) else { return false }

                var overrideInstructionPointerIncrement = false
                defer { self.instructionPointer += overrideInstructionPointerIncrement ? 0 : 1 }

                let instruction = instructions[instructionPointer]
                switch instruction {
                case .snd(let operand):
                    sendClosure?(getValue(for: operand))
                case .set(let register, let operand):
                    registers[register] = getValue(for: operand)
                case .add(let register, let operand):
                    registers[register, default: 0] += getValue(for: operand)
                case .mul(let register, let operand):
                    registers[register, default: 0] *= getValue(for: operand)
                case .mod(let register, let operand):
                    registers[register, default: 0] %= getValue(for: operand)
                case .rcv(let operand):
                    guard !queue.isEmpty else {
                        overrideInstructionPointerIncrement = true
                        return false
                    }

                    switch operand {
                    case .register(let r):
                        registers[r] = queue.removeFirst()
                    case .value:
                        fatalError("Should never happen.")
                    }
                case .jgz(let operand, let offset):
                    if getValue(for: operand) > 0 {
                        self.instructionPointer += getValue(for: offset)
                        overrideInstructionPointerIncrement = true
                    }
                }
                return true
            }

            private func getValue(for operand: Instruction.Operand) -> Int {
                switch operand {
                case .register(let r):
                    return registers[r, default: 0]
                case .value(let v):
                    return v
                }
            }
        }

        let machineA: Machine
        let machineB: Machine
        var sentValues: Int

        init(instructions: [Instruction]) {
            self.machineA = Machine(instructions: instructions, programID: 0)
            self.machineB = Machine(instructions: instructions, programID: 1)
            self.sentValues = 0

            self.machineA.sendClosure = { [weak self] in
                self?.machineB.queue.append($0)
            }
            self.machineB.sendClosure = { [weak self] in
                self?.machineA.queue.append($0)
                self?.sentValues += 1
            }
        }

        func run() -> Int {
            while machineA.tick() || machineB.tick() {}
            return sentValues
        }
    }

    private var instructions: [Instruction] = []

    override func didLoadFunction() {
        instructions = defaultInputFileString.loadAsStringArray()
            .map { Instruction($0) }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let machine = Machine(instructions: instructions)
        return machine.runUntilFirstRecoveredFrequency()!
    }

    override func solveFunction2() -> CustomStringConvertible {
        let duetMachine = DuetMachine(instructions: instructions)
        return duetMachine.run()
    }
}


extension Solver_2017_18: TestableDay {
    func runTests() {
        let input = """
set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2
"""
            .components(separatedBy: .newlines)

        let instructions = input.map { Instruction($0) }
        let machine = Machine(instructions: instructions)
        let recoveredFrequency = machine.runUntilFirstRecoveredFrequency()
        let expectedFrequency = 4
        assert(recoveredFrequency == expectedFrequency)

        let program2 = """
snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d
"""
            .components(separatedBy: .newlines)
            .map { Instruction($0) }

        let duetMachine = DuetMachine(instructions: program2)
        let sentValues = duetMachine.run()
        let expectedSentValues = 3
        assert(sentValues == expectedSentValues)
    }
}
