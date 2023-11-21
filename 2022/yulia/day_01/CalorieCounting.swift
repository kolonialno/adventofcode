//
//  CalorieCounting.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 01/12/2022.
//

import Foundation

struct CalorieCounting {

    func part1(_ input: String) -> Int {
        return input.components(separatedBy: "\n\n")
            .map {
                $0.components(separatedBy: "\n")
                    .map { Int($0)! }
                    .reduce(0, +)
            }
            .max()!
    }

    func part2(_ input: String) -> Int {
        return input.components(separatedBy: "\n\n")
            .map {
                $0.components(separatedBy: "\n")
                    .map { Int($0)! }
                    .reduce(0, +)
            }
            .sorted(by: { $0 > $1 })
            .prefix(3)
            .reduce(0, +)
    }
}
