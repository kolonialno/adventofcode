//
//  Solver_2022_11.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 11/12/2022.
//

import Foundation

class Solver_2022_11: Solver {
    enum WorryMode {
        case worried
        case thisIsFine(customWorryFactor: Int)
    }

    enum MonkeyOperatorValue {
        case old
        case raw(value: Int)

        init(string: String) {
            if string == "old" {
                self = .old
            } else {
                self = .raw(value: Int(string)!)
            }
        }

        func getValue(for item: MonkeyItem) -> Int {
            switch self {
            case .old: return item.value
            case .raw(let value): return value
            }
        }
    }

    enum MonkeyOperator: String {
        case plus = "+"
        case multiplied = "*"
    }

    struct MonkeyOperation {
        let a: MonkeyOperatorValue
        let b: MonkeyOperatorValue
        let op: MonkeyOperator

        func getNewValue(for item: MonkeyItem) -> Int {
            let aV = a.getValue(for: item)
            let bV = b.getValue(for: item)

            switch op {
            case .plus: return aV + bV
            case .multiplied: return aV * bV
            }
        }
    }

    class MonkeyItem {
        var value: Int

        init(value: Int) {
            self.value = value
        }
    }

    class Monkey {
        let monkeyOperation: MonkeyOperation
        var numInspects = 0
        var items: [MonkeyItem]
        var divisibleTest: Int
        var trueMonkeyIndex: Int
        var falseMonkeyIndex: Int

        init(string: String) {
            let split = string
                .components(separatedBy: .newlines)
                .map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }

            // Items
            let itemListSplit = split[1]
                .components(separatedBy: ": ")
            self.items = itemListSplit[1] .components(separatedBy: ", ")
                .map { Int($0)! }
                .map { MonkeyItem(value: $0) }

            // Operation
            let operationSplit = split[2]
                .components(separatedBy: " ")
            let operatorValueA = MonkeyOperatorValue(string: operationSplit[3])
            let operatorValueB = MonkeyOperatorValue(string: operationSplit[5])
            let op = MonkeyOperator(rawValue: operationSplit[4])!
            self.monkeyOperation = .init(a: operatorValueA, b: operatorValueB, op: op)

            // Divisible test
            let divisibleSplit = split[3]
                .components(separatedBy: " ")
            self.divisibleTest = Int(divisibleSplit.last!)!

            // True monkey
            let trueSplit = split[4]
                .components(separatedBy: " ")
            self.trueMonkeyIndex = Int(trueSplit.last!)!

            // False monkey
            let falseSplit = split[5]
                .components(separatedBy: " ")
            self.falseMonkeyIndex = Int(falseSplit.last!)!
        }

        func inspect(item: MonkeyItem, worryMode: WorryMode) {
            let newValue = monkeyOperation.getNewValue(for: item)
            item.value = newValue

            switch worryMode {
            case .worried:
                item.value /= 3
            case .thisIsFine(let customWorryFactor):
                item.value %= customWorryFactor
            }

            numInspects += 1
        }

        /// Returns which monkey index to throw to
        func test(item: MonkeyItem) -> Int {
            return item.value.isMultiple(of: divisibleTest) ? trueMonkeyIndex : falseMonkeyIndex
        }

        func addItem(item: MonkeyItem) {
            items.append(item)
        }
    }

    class MonkeyManager {
        let monkeys: [Monkey]
        let worryMode: WorryMode

        init(string: String, isWorried: Bool) {
            let monkeyStrings = string.components(separatedBy: "\n\n")
            let monkeys = monkeyStrings.map { Monkey(string: $0) }

            let customWorryFactor = monkeys
                .map { $0.divisibleTest }
                .reduce(1, *)

            self.monkeys = monkeys
            self.worryMode = isWorried ? .worried : .thisIsFine(customWorryFactor: customWorryFactor)
        }

        func play(numRounds: Int) {
            for _ in 0..<numRounds {
                playRound()
            }
        }

        private func playRound() {
            monkeys.forEach { monkey in
                monkey.items.forEach { item in
                    monkey.inspect(item: item, worryMode: worryMode)
                    let newMonkeyIndex = monkey.test(item: item)
                    monkeys[newMonkeyIndex].addItem(item: item)
                }
                monkey.items = []
            }
        }

        func getLevelOfMonkeyBusiness() -> Int {
            monkeys
                .map { $0.numInspects }
                .sorted()
                .suffix(2)
                .reduce(1, *)
        }
    }

    private var input = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString()
    }

    override func solveFunction1() -> String {
        let manager = MonkeyManager(string: input, isWorried: true)
        manager.play(numRounds: 20)
        let result = manager.getLevelOfMonkeyBusiness()

        return "\(result)"
    }

    override func solveFunction2() -> String {
        let manager = MonkeyManager(string: input, isWorried: false)
        manager.play(numRounds: 10_000)
        let result = manager.getLevelOfMonkeyBusiness()

        return "\(result)"
    }
}

extension Solver_2022_11: TestableDay {
    func runTests() {
        let testInput = defaultTestInputString(suffix: "a").loadAsTextString()
        let manager1 = MonkeyManager(string: testInput, isWorried: true)

        manager1.play(numRounds: 20)
        assert(manager1.monkeys[0].numInspects == 101)
        assert(manager1.monkeys[1].numInspects == 95)
        assert(manager1.monkeys[2].numInspects == 7)
        assert(manager1.monkeys[3].numInspects == 105)
        assert(manager1.getLevelOfMonkeyBusiness() == 10605)

        let manager2 = MonkeyManager(string: testInput, isWorried: false)
        manager2.play(numRounds: 20)
        assert(manager2.monkeys[0].numInspects == 99)
        assert(manager2.monkeys[1].numInspects == 97)
        assert(manager2.monkeys[2].numInspects == 8)
        assert(manager2.monkeys[3].numInspects == 103)

        manager2.play(numRounds: 1_000 - 20)
        assert(manager2.monkeys[0].numInspects == 5_204)
        assert(manager2.monkeys[1].numInspects == 4_792)
        assert(manager2.monkeys[2].numInspects == 199)
        assert(manager2.monkeys[3].numInspects == 5_192)

        manager2.play(numRounds: 10_000 - 1_000)
        assert(manager2.monkeys[0].numInspects == 52_166)
        assert(manager2.monkeys[1].numInspects == 47_830)
        assert(manager2.monkeys[2].numInspects == 1_938)
        assert(manager2.monkeys[3].numInspects == 52_013)

        assert(manager2.getLevelOfMonkeyBusiness() == 2_713_310_158)
    }
}
