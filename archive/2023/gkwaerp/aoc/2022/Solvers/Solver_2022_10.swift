//
//  Solver_2022_10.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 10/12/2022.
//

import Foundation

class Solver_2022_10: Solver {
    override var solveType2: SolverView.SolveType {
        .image
    }

    class CRT {
        enum Instruction {
            case noop
            case add(register: String, value: Int)

            var numCyclesToComplete: Int {
                switch self {
                case .noop: return 1
                case .add: return 2
                }
            }

            init(string: String) {
                if string == "noop" {
                    self = .noop
                } else if string.hasPrefix("add") {
                    let split = string
                        .replacingOccurrences(of: "add", with: "")
                        .components(separatedBy: " ")

                    let register = split[0]
                    let value = Int(split[1])!

                    self = .add(register: register, value: value)
                } else {
                    fatalError("Invalid instruction: \(string)")
                }
            }
        }

        private let program: [Instruction]

        private(set) var cycle: Int
        private(set) var registers: [String: Int]
        private var instructionIndex: Int
        private var instructionTimer: Int
        private var currentInstruction: Instruction?

        private(set) var signalStrengths: [Int]
        private(set) var screen: StringGrid

        static let spriteRegister = "x"

        init(program: [Instruction]) {
            self.program = program
            self.cycle = 0
            self.registers = [Self.spriteRegister: 1]
            self.instructionTimer = 0
            self.instructionIndex = 0
            self.currentInstruction = nil
            self.signalStrengths = []
            self.screen = StringGrid(size: .init(x: 40, y: 6), fillWith: "-")
        }

        func run() {
            while instructionIndex < program.count || instructionTimer > 0 {
                preTick()
                tick()
                postTick()
            }
        }

        private func preTick() {
            if currentInstruction == nil, instructionIndex < program.count {
                currentInstruction = program[instructionIndex]
                instructionTimer = currentInstruction!.numCyclesToComplete
            }
        }

        private func tick() {
            cycle += 1
            instructionTimer -= 1
        }

        private func postTick() {
            measureSignalStrength()
            draw()

            if let currentInstruction, instructionTimer == 0 {
                execute(instruction: currentInstruction)
            }
        }

        private func measureSignalStrength() {
            guard cycle == 20 || (cycle - 20).isMultiple(of: 40) else {
                return
            }

            signalStrengths.append(getRegisterValue(register: Self.spriteRegister) * cycle)
        }

        private func draw() {
            let spriteWidth = 3
            let writePos = IntPoint(x: (cycle - 1) % 40,
                                    y: (cycle - 1) / 40)

            let spriteDisplayed = abs(getRegisterValue(register: Self.spriteRegister) - writePos.x) <= (spriteWidth / 2)
            let writeValue = spriteDisplayed ? "🟧" : "🟦"

            screen.setValue(at: writePos, to: writeValue)
        }

        private func execute(instruction: Instruction) {
            switch instruction {
            case .noop:
                break
            case .add(let register, let value):
                let newValue = getRegisterValue(register: register) + value
                setRegisterValue(register: register, value: newValue)
            }

            currentInstruction = nil
            instructionIndex += 1
        }

        private func getRegisterValue(register: String) -> Int {
            registers[register]!
        }

        private func setRegisterValue(register: String, value: Int) {
            registers[register] = value
        }
    }

    func getSignalStrengthSum(crt: CRT) -> Int {
        crt.signalStrengths.reduce(0, +)
    }

    func readScreen(crt: CRT) -> String {
        crt.screen.asText(printClosure: StringGrid.defaultPrintClosure())
    }

    private var program: [CRT.Instruction] = []

    override func didLoadFunction() {
        self.program = defaultInputFileString
            .loadAsStringArray()
            .map { CRT.Instruction(string: $0) }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let crt = CRT(program: program)
        crt.run()

        let signalStrengthSum = getSignalStrengthSum(crt: crt)
        return "\(signalStrengthSum)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let crt = CRT(program: program)
        crt.run()

        let output = readScreen(crt: crt)
        return output
    }
}
