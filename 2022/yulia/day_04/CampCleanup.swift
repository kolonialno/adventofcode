//
//  CampCleanup.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 04/12/2022.
//

import Foundation

struct CampCleanup {

    enum Check {
        case contain
        case overlap
    }

    func part1(_ input: String) -> Int {
        return findOverlapOrContain(input, check: .contain)
    }

    func part2(_ input: String) -> Int {
        return findOverlapOrContain(input, check: .overlap)
    }

    private func findOverlapOrContain(_ input: String, check: Check) -> Int {
        let pairs = input.components(separatedBy: "\n")
        return pairs.map { pairStr -> Int in

            let components = pairStr.components(separatedBy: ",")
            let comp1 = components[0].components(separatedBy: "-")
            let comp2 = components[1].components(separatedBy: "-")
            let range1 = Int(comp1[0])!...Int(comp1[1])!
            let range2 = Int(comp2[0])!...Int(comp2[1])!

            switch check {
            case .contain:
                if range1.contains(range2) || range2.contains(range1) {
                    return 1
                } else {
                    return 0
                }
            case .overlap:
                if range1.overlaps(range2) {
                    return 1
                } else {
                    return 0
                }
            }

        }
        .reduce(0, +)
    }
}
