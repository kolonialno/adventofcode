//
//  Solver_2023_03.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 03/12/2023.
//

import Foundation

final class Solver_2023_03: Solver {
    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        getSumOfPartNumbers(input)
    }

    override func solveFunction2() -> CustomStringConvertible {
        getSumOfGearRatios(input)
    }

    func getSumOfPartNumbers(_ input: [String]) -> Int {
        var partNumbers: [Int] = []
        var currentNumber = 0
        var isAdjacentToSymbol = false
        let grid = StringGrid(stringArray: input)

        let addAndResetIfApplicable: ((Int) -> Void) = { number in
            if currentNumber != 0 && isAdjacentToSymbol {
                partNumbers.append(currentNumber)
            }
            isAdjacentToSymbol = false
            currentNumber = 0
        }

        for y in 0..<grid.size.y {
            for x in 0..<grid.size.x {
                let point = IntPoint(x: x, y: y)
                let value = grid.getValue(at: point)!

                if let intValue = value.intValue {
                    currentNumber *= 10
                    currentNumber += intValue

                    let neighbors = IntPoint.allDirectionOffsets.map { point + $0 }
                    neighbors.forEach { neighbor in
                        guard let nValue = grid.getValue(at: neighbor) else { return }
                        if nValue.intValue == nil && nValue != "." {
                            isAdjacentToSymbol = true
                        }
                    }
                } else {
                    addAndResetIfApplicable(currentNumber)
                }
            }
            addAndResetIfApplicable(currentNumber)
        }

        return partNumbers.reduce(0, +)
    }

    func getSumOfGearRatios(_ input: [String]) -> Int {
        var currentNumber = 0
        var adjacentGearSymbolPositions: Set<IntPoint> = []
        var gearPointDict: [IntPoint: [Int]] = [:]

        let addAndResetIfApplicable: ((Int) -> Void) = { number in
            if currentNumber != 0 && !adjacentGearSymbolPositions.isEmpty {
                adjacentGearSymbolPositions.forEach { gearPoint in
                    gearPointDict[gearPoint, default: []].append(currentNumber)
                }
            }
            adjacentGearSymbolPositions.removeAll()
            currentNumber = 0
        }

        let grid = StringGrid(stringArray: input)
        for y in 0..<grid.size.y {
            for x in 0..<grid.size.x {
                let point = IntPoint(x: x, y: y)
                let value = grid.getValue(at: point)!

                if let intValue = value.intValue {
                    currentNumber *= 10
                    currentNumber += intValue

                    let neighbors = IntPoint.allDirectionOffsets.map { point + $0 }
                    adjacentGearSymbolPositions.formUnion(neighbors.filter( { grid.getValue(at: $0) == "*" }))
                } else {
                    addAndResetIfApplicable(currentNumber)
                }
            }
            addAndResetIfApplicable(currentNumber)
        }

        return gearPointDict.values
            .filter { $0.count == 2 }
            .map { $0.reduce(1, *) }
            .reduce(0, +)
    }
}

extension Solver_2023_03: TestableDay {
    func runTests() {
        let input = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""
            .components(separatedBy: .newlines)

        let result1 = getSumOfPartNumbers(input)
        let expected1 = 4361
        assert(result1 == expected1)

        let result2 = getSumOfGearRatios(input)
        let expected2 = 467835
        assert(result2 == expected2)
    }
}
