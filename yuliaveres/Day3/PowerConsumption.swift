//
//  PowerConsumption.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 03/12/2021.
//

import Foundation

struct PowerConsumptionRate {

    func rate(_ input: [Int], _ rate: Kind, maxNumber: Int) throws -> Int {
        var binary = ""
        for countOf1 in input {
            if rate.isValid(countOf1, maxNumber) {
                binary.append("1")
            } else {
                binary.append("0")
            }
        }

        guard let number = Int(binary, radix: 2) else {
            throw BinaryDiagnostic.Error.invlalidBinaryNumber
        }

        return number
    }

    private func isGamma(count: Int, maxCount: Int) -> Bool {
       return max(maxCount - count, count) == count
    }

    private func isEpsilob(count: Int, maxCount: Int) -> Bool {
        return min(maxCount - count, count) == count
    }
}

extension PowerConsumptionRate {

    enum Kind {
        case gamma
        case epsilon

        func isValid(_ count: Int, _ maxCount: Int) -> Bool {
            switch self {
            case .gamma:
                return max(maxCount - count, count) == count
            case .epsilon:
                return min(maxCount - count, count) == count
            }
        }
    }
}
