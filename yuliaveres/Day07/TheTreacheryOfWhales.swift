//
//  TheTreacheryOfWhales.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 07/12/2021.
//

import Foundation

struct TheTreacheryOfWhales {

    func part1(_ input: String) -> Int {
        let positions = input.components(separatedBy: ",").compactMap { Int($0) }

        var map: [Int: Int] = [:]

        for (idx, position) in positions.enumerated() {
            var rest = positions
            rest.remove(at: idx)
            map[idx] = rest.map { abs(position - $0) }.reduce(0, +)
        }

        return map.values.min()!
    }

    func part2(_ input: String) -> Int {
        let positions = input.components(separatedBy: ",").compactMap { Int($0) }

        var map: [Int: Int] = [:]

        for another in positions.min()!...positions.max()! {
            var total = 0

            for crabPosition in positions {
                let steps = abs(crabPosition - another)
                guard steps > 0 else { continue }

                total += steps * (steps + 1) / 2
            }
            map[another] = total
        }

        return map.values.min()!
    }
}
