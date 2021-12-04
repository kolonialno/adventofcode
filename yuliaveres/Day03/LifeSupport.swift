//
//  LifeSupport.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 03/12/2021.
//

import Foundation

struct LifeSupport {

    func rating(_ input: String, _ kind: Kind) throws -> Int {
        var components = input.components(separatedBy: "\n")

        var index = 0

        while components.count > 1 {
            print(index)

            var countOf1 = 0
            var countOf0 = 0

            components.forEach {

               let char = $0[$0.index($0.startIndex, offsetBy: index)]

                if char == "1" {
                    countOf1 += 1
                } else {
                    countOf0 += 1
                }
            }

            switch kind {
            case .oxygenGenerator:
                if countOf0 > countOf1 {
                    components = components.filter { $0.char(index) == "0" }
                } else {
                    components = components.filter { $0.char(index) == "1" }
                }
            case .co2Scrubber:
                if countOf1 < countOf0 {
                    components = components.filter { $0.char(index) == "1" }
                } else {
                    components = components.filter { $0.char(index) == "0" }
                }
            }

            index += 1
        }

        let binary = components[0]

        guard let number = Int(binary, radix: 2) else {
            throw BinaryDiagnostic.Error.invlalidBinaryNumber
        }

        return number
    }
}

extension LifeSupport {


    enum Kind {
        case oxygenGenerator
        case co2Scrubber
    }
}
