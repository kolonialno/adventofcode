//
//  MonkeyInTheMiddle.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 11/12/2022.
//

import Foundation

final class MonkeyInTheMiddle {

    struct Monkey: Hashable {
        let id: Int
        let operation: String
        let testDivisible: Int
        let positiveMonkeyPass: Int
        let negativeMonkeyPass: Int

        func passToId(_ item: Int) -> Int {
            let monkeyIdToPass: Int
            if (item % testDivisible) == 0 {
                monkeyIdToPass = positiveMonkeyPass
            } else {
                monkeyIdToPass = negativeMonkeyPass
            }

            return monkeyIdToPass
        }
    }

    var monkeysAndItems: [Monkey: [Int]] = [:]
    var monkeyIdToInspected: [Int: Int] = [:]

    var monkeyToSmallerValue: [[Int]] = []
    var monkeyToIndexesMap: [Monkey: [Int]] = [:]
    var originalValues: [Int] = []

    func part1(_ input: String, rounds: Int) -> Int {
        parse(input)

        let monkeys = monkeysAndItems.keys.sorted(by: { $0.id < $1.id })

        for _ in Array(1...rounds) {
            for monkey in monkeys {
                let items = monkeysAndItems[monkey]!
                let inspectedCount: Int
                if let inspected = monkeyIdToInspected[monkey.id] {
                    inspectedCount = inspected + items.count
                } else {
                    inspectedCount = items.count
                }

                monkeyIdToInspected[monkey.id] = inspectedCount

                for item in items {
                    let boredWorry = newWorryLevel(of: item, after: monkey) / 3
                    passValue(boredWorry, from: monkey, in: &monkeysAndItems)
                }
            }
        }

        return monkeyIdToInspected.sorted(by: { $0.value > $1.value }).prefix(2).map { $0.value }.reduce(1, *)
    }

    func part2(_ input: String, rounds: Int) -> Int {
        parse(input)
        let monkeys = monkeysAndItems.keys.sorted(by: { $0.id < $1.id })

        for monkey in monkeys {
            var itemsIndexes: [Int] = []
            for item in monkeysAndItems[monkey]! {
                itemsIndexes.append(originalValues.count)
                originalValues.append(item)
            }
            monkeyToIndexesMap[monkey] = itemsIndexes
        }

        monkeyToSmallerValue = monkeys.map { monkey in originalValues.map { $0 % monkey.testDivisible } }

        for _ in Array(1...rounds) {
            for monkey in monkeys {
                let itemsIdx = monkeyToIndexesMap[monkey]!
                let inspectedCount: Int
                if let inspected = monkeyIdToInspected[monkey.id] {
                    inspectedCount = inspected + itemsIdx.count
                } else {
                    inspectedCount = itemsIdx.count
                }

                monkeyIdToInspected[monkey.id] = inspectedCount
                updateItemsIdx(ownedBy: monkey, for: monkeys, itemsIdx: itemsIdx)
            }
        }

        return monkeyIdToInspected.sorted(by: { $0.value > $1.value }).prefix(2).map { $0.value }.reduce(1, *)
    }

    private func newWorryLevel(of item: Int, after monkey: Monkey) -> Int {
        let operation = monkey.operation
        let newWorry: Int
        if operation.hasPrefix("+ ") {
            let operationValue = Int(operation.components(separatedBy: "+ ")[1])!
            newWorry = item + operationValue
        } else if operation.hasPrefix("* ") {
            let operationValue: Int

            if let intValue = Int(operation.components(separatedBy: "* ")[1]) {
                operationValue = intValue
            } else {
                operationValue = item
            }

            newWorry = item * operationValue
        } else {
            fatalError("Unexpected operation")
        }

        return newWorry
    }

    private func updateSmallerValues(for operation: String, monkeys: [Monkey], itemIdx: Int) {
        if operation.hasPrefix("+ ") {
            let operationValue = Int(operation.components(separatedBy: "+ ")[1])!

            for monkey2 in monkeys {
                let smallerValue = monkeyToSmallerValue[monkey2.id][itemIdx]
                let newSmallerValue = (smallerValue + operationValue) % monkey2.testDivisible
                monkeyToSmallerValue[monkey2.id][itemIdx] = newSmallerValue
            }
        } else if operation.hasPrefix("* ") {

            for monkey2 in monkeys {
                let operationValue: Int
                let smallerValue = monkeyToSmallerValue[monkey2.id][itemIdx]
                if let intValue = Int(operation.components(separatedBy: "* ")[1]) {
                    operationValue = intValue
                } else {
                    operationValue = smallerValue
                }

                let newSmallerValue = (smallerValue * operationValue) % monkey2.testDivisible
                monkeyToSmallerValue[monkey2.id][itemIdx] = newSmallerValue
            }

        } else {
            fatalError("Unexpected operation")
        }
    }

    private func updateItemsIdx(ownedBy monkey: Monkey, for otherMonkeys: [Monkey], itemsIdx: [Int]) {
        for itemIdx in itemsIdx {
            let item = monkeyToSmallerValue[monkey.id][itemIdx]
            let operation = monkey.operation
            updateSmallerValues(for: operation, monkeys: otherMonkeys, itemIdx: itemIdx)

            let boredWorry = monkeyToSmallerValue[monkey.id][itemIdx]
            let monkeyIdToPass = monkey.passToId(boredWorry)

            guard let monkeyToPass = monkeyToIndexesMap.first(where: { $0.key.id == monkeyIdToPass })?.key else { fatalError("Failed to find monkey with id: \(monkeyIdToPass)") }
            var array: [Int]
            if let existing = monkeyToIndexesMap[monkeyToPass] {
                array = existing
            } else {
                array = []
            }

            array.append(itemIdx)
            let newValuesOfCurrentMonkey = Array(monkeyToIndexesMap[monkey]!.dropLast(1))
            monkeyToIndexesMap[monkeyToPass] = array
            monkeyToIndexesMap[monkey] = newValuesOfCurrentMonkey
        }
    }

    private func passValue(_ item: Int, from monkey: Monkey, in currentMap: inout [Monkey: [Int]]) {
        let monkeyIdToPass = monkey.passToId(item)
        guard let monkeyToPass = currentMap.first(where: { $0.key.id == monkeyIdToPass })?.key else { fatalError("Failed to find monkey with id: \(monkeyIdToPass)") }

        var array: [Int]
        if let existing = currentMap[monkeyToPass] {
            array = existing
        } else {
            array = []
        }

        array.append(item)
        let newValuesOfCurrentMonkey = Array(currentMap[monkey]!.dropLast(1))
        currentMap[monkeyToPass] = array
        currentMap[monkey] = newValuesOfCurrentMonkey
    }

    private func parse(_ input: String) {
        let comps = input.components(separatedBy: "\n\n")

        for monkeyInfo in comps {
            let lines = monkeyInfo.components(separatedBy: "\n")

            let monkeyId = Int(lines[0].dropLast(1).components(separatedBy: " ")[1])!

            let items = lines[1].components(separatedBy: "Starting items: ")[1].components(separatedBy: ", ").map { Int($0)! }
            let operation = lines[2].components(separatedBy: "= old ")[1]
            let testDivisble = Int(lines[3].components(separatedBy: " by ")[1])!

            let positiveMonkeyPass = Int(lines[4].components(separatedBy: " to monkey ")[1])!
            let negativeMonkeyPass = Int(lines[5].components(separatedBy: " to monkey ")[1])!

            let monkey: Monkey = .init(id: monkeyId,
                                       operation: operation,
                                       testDivisible: testDivisble,
                                       positiveMonkeyPass: positiveMonkeyPass,
                                       negativeMonkeyPass: negativeMonkeyPass)

            monkeysAndItems[monkey] = items
        }
    }
}
