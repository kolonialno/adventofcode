//
//  Lanternfish.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 06/12/2021.
//

import Foundation

struct Lanternfish {


    let schoolOfFish: [Int]

    init(input: String) {
        schoolOfFish = input.components(separatedBy: ",").compactMap { Int($0) }
    }

    func part1(for days: Int) -> Int {
        let input = schoolOfFish

        var fish = Array(repeating: 0, count: 9)
        input.forEach { f in
            fish[f] += 1
        }

        for d in 0..<days {
            fish[(d + 7) % 9] += fish[d % 9]
        }

        return fish.reduce(0, +)
    }
}
