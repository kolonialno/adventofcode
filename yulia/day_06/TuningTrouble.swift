//
//  TuningTrouble.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 06/12/2022.
//

import Foundation

struct TuningTrouble {

    func part1(_ input: String) -> Int {
        return lastMarkerCharIdx(for: input, markerLength: 4)
    }

    func part2(_ input: String) -> Int {
        return lastMarkerCharIdx(for: input, markerLength: 14)
    }

    private func lastMarkerCharIdx(for input: String, markerLength: Int) -> Int {
        let chars = [Character](input)
        var indexMarkerEndIdx: Int = 0
        for (idx, _) in chars.enumerated() {
            let endIdx = idx + markerLength - 1
            guard endIdx <= chars.count - 1 else { continue }

            let subChars = chars[idx...endIdx]
            if Set(subChars).count == subChars.count {
                indexMarkerEndIdx = endIdx
                break
            }
        }

        return indexMarkerEndIdx + 1
    }
}
