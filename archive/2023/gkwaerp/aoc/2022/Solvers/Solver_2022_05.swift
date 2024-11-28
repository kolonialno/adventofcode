//
//  Solver_2022_05.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 05/12/2022.
//

import Foundation

class Solver_2022_05: Solver {
    enum Instruction {
        case moveIndividual(num: Int, from: Int, to: Int)
        case moveMulti(num: Int, from: Int, to: Int)

        init(_ string: String, allowsMultiMove: Bool) {
            let split = string.components(separatedBy: " ")
            let num = Int(split[1])!
            let from = Int(split[3])! - 1
            let to = Int(split[5])! - 1

            if allowsMultiMove {
                self = .moveMulti(num: num, from: from, to: to)
            } else {
                self = .moveIndividual(num: num, from: from, to: to)
            }
        }
    }

    class StackManager {
        private(set) var stacks: [[String]]

        private var instructions: [Instruction]

        init(_ string: String, allowsMultiMove: Bool) {
            let mainSplit = string.components(separatedBy: "\n\n")

            self.instructions = mainSplit[1]
                .components(separatedBy: .newlines)
                .filter { !$0.isEmpty }
                .map { .init($0, allowsMultiMove: allowsMultiMove) }

            let stackInfo = Array(mainSplit[0]
                .components(separatedBy: .newlines)
                .map { $0.convertToStringArray() }
                .dropLast() // Just a list of indices, not needed
                .reversed())

            self.stacks = []

            stackInfo.forEach { row in
                stride(from: 1, to: row.count, by: 4).forEach { readIndex in
                    let char = row[readIndex]

                    guard char != " " else {
                        return
                    }

                    let writeIndex = (readIndex - 1) / 4

                    while self.stacks.count <= writeIndex {
                        self.stacks.append([])
                    }

                    self.stacks[writeIndex].append(char)
                }
            }
        }

        func performInstructions() -> String {
            instructions.forEach { perform(instruction: $0) }

            return getTops()
        }

        private func perform(instruction: Instruction) {
            switch instruction {
            case .moveIndividual(let num, let from, let to):
                (0..<num).forEach { _ in stacks[to].append(stacks[from].popLast()!) }
            case .moveMulti(let num, let from, let to):
                stacks[to].append(contentsOf: stacks[from][(stacks[from].count - num)...])
                stacks[from] = Array(stacks[from].dropLast(num))
            }
        }

        private func getTops() -> String {
            stacks
                .compactMap { $0.last }
                .joined()
        }
    }

    private var input: String = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString(trimming: false)
    }

    override func solveFunction1() -> CustomStringConvertible {
        let stackManager = StackManager(input, allowsMultiMove: false)
        return stackManager.performInstructions()
    }

    override func solveFunction2() -> CustomStringConvertible {
        let stackManager = StackManager(input, allowsMultiMove: true)
        return stackManager.performInstructions()
    }
}

extension Solver_2022_05: TestableDay {
    func runTests() {
        let testInput = """
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""
        let stackManager1 = StackManager(testInput, allowsMultiMove: false)
        assert(stackManager1.stacks == [["Z", "N"], ["M", "C", "D"], ["P"]])

        let tops1 = stackManager1.performInstructions()
        assert(tops1 == "CMZ")

        let stackManager2 = StackManager(testInput, allowsMultiMove: true)
        let tops2 = stackManager2.performInstructions()
        assert(tops2 == "MCD")
    }
}
