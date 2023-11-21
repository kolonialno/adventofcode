//
//  Solver_2022_21.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 21/12/2022.
//

import Foundation

class Solver_2022_21: Solver {
    class MonkeyMather {
        enum MonkeyOperation {
            case number(number: Int)
            case add(monkeyA: String, monkeyB: String)
            case subtract(monkeyA: String, monkeyB: String)
            case divide(monkeyA: String, monkeyB: String)
            case multiply(monkeyA: String, monkeyB: String)
        }

        class Monkey {
            let name: String
            var _operation: MonkeyOperation

            init(string: String) {
                let split = string
                    .replacingOccurrences(of: ":", with: "")
                    .components(separatedBy: " ")

                let operation: MonkeyOperation
                if split.count == 2 {
                    operation = .number(number: Int(split[1])!)
                } else {
                    let monkeyA = split[1]
                    let monkeyB = split[3]
                    switch split[2] {
                    case "+":
                        operation = .add(monkeyA: monkeyA, monkeyB: monkeyB)
                    case "-":
                        operation = .subtract(monkeyA: monkeyA, monkeyB: monkeyB)
                    case "*":
                        operation = .multiply(monkeyA: monkeyA, monkeyB: monkeyB)
                    case "/":
                        operation = .divide(monkeyA: monkeyA, monkeyB: monkeyB)
                    default:
                        fatalError("Invalid monkey operation")
                    }
                }

                self.name = split[0]
                self._operation = operation
            }
        }
        /// Monkey name --> Value
        var cache: [String: Int]

        /// Monkey name --> operation
        var monkeys: [String: MonkeyOperation]

        init(strings: [String]) {
            self.cache = [:]
            self.monkeys = [:]

            let monkeys = strings.map { Monkey(string: $0) }
            monkeys.forEach{ self.monkeys[$0.name] = $0._operation }
        }

        func evaluate(monkey: String) -> Int {
            if let cached = cache[monkey] {
                return cached
            }

            guard let monkeyOperation = monkeys[monkey] else { fatalError() }
            let result: Int

            switch monkeyOperation {
            case .number(let number):
                result = number
            case .add(let monkeyA, let monkeyB):
                result = evaluate(monkey: monkeyA) + evaluate(monkey: monkeyB)
            case .subtract(let monkeyA, let monkeyB):
                result = evaluate(monkey: monkeyA) - evaluate(monkey: monkeyB)
            case .multiply(let monkeyA, let monkeyB):
                result = evaluate(monkey: monkeyA) * evaluate(monkey: monkeyB)
            case .divide(let monkeyA, let monkeyB):
                result = evaluate(monkey: monkeyA) / evaluate(monkey: monkeyB)
            }

            cache[monkey] = result
            return result
        }
    }

    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextStringArray()
    }

    override func solveFunction1() -> String {
        let monkeyMather = MonkeyMather(strings: input)
        let result = monkeyMather.evaluate(monkey: "root")
        return "\(result)"
    }

    override func solveFunction2() -> String {
        "Solved manually"
    }
}

extension Solver_2022_21: TestableDay {
    func runTests() {
        let testInput = defaultTestInputString(suffix: "a").loadAsTextStringArray()
        let monkeyMather = MonkeyMather(strings: testInput)
        let result = monkeyMather.evaluate(monkey: "root")
        assert(result == 152)
    }
}
