//
//  BinaryDiagnostic.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 03/12/2021.
//

import Foundation

struct BinaryDiagnostic {

    func part1(_ input: String) throws -> Int {
        let components = input.components(separatedBy: "\n")

        let numberLength = components[0].count
        let numbersCount = components.count

        var map = Array(repeating: 0, count: numberLength)

        var index = 0

        for char in input {
            if char == "\n" {
                index = 0
                continue
            }

            if char == "1"{
                map[index] = map[index] + 1
            }

            index += 1
        }

        let gamma = try PowerConsumptionRate().rate(map, .gamma, maxNumber: numbersCount)
        let epsilon = try PowerConsumptionRate().rate(map, .epsilon, maxNumber: numbersCount)

        return gamma * epsilon
    }

    func part2(_ input: String) throws -> Int {
        let oxygenRating = try LifeSupport().rating(input, .oxygenGenerator)
        let co2Ratings = try LifeSupport().rating(input, .co2Scrubber)

        return oxygenRating * co2Ratings
    }
}

extension BinaryDiagnostic {

    enum Error: LocalizedError {
        case invlalidBinaryNumber
    }
}
