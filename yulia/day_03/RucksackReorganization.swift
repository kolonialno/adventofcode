//
//  RucksackReorganization.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 03/12/2022.
//

import Foundation

struct RucksackReorganization {

    let rangeScores: Zip2Sequence<[Character], [Int]>

    init() {
        let letters = (97...122).map({ Character(UnicodeScalar($0) )}) + (65...90).map({ Character(UnicodeScalar($0) )})
        let scores = Array((1...26)) + Array((27...52))

        self.rangeScores = zip(letters, scores)
    }

    private func score(for char: Character) -> Int {
        let score: Int
        if let result = rangeScores.first(where: { $0.0 == char }) {
            score = result.1
        } else {
            fatalError("Common item not found")
        }

        return score
    }

    func part1(_ input: String) -> Int {
        let rucksacks = input.components(separatedBy: "\n")
        return rucksacks.map { content -> Int in
            let middle = content.index(content.startIndex, offsetBy: content.count / 2)
            let half1 = content.prefix(upTo: middle)
            let half2 = content.suffix(from: middle)
            let common = Set(half1).intersection(Set(half2)).first!

            return score(for: common)
        }
        .reduce(0, +)
    }

    func part2(_ input: String) -> Int {
        let rucksacks = input.components(separatedBy: "\n")
        let groups = stride(from: 0, to: rucksacks.count, by: 3).map {
            Array(rucksacks[$0..<min($0 + 3, rucksacks.count)])
        }

        return groups.map { group -> Int in
            let common = Set(group[0])
                .intersection(Set(group[1]))
                .intersection(Set(group[2])).first!

            return score(for: common)
        }
        .reduce(0, +)
    }
}
