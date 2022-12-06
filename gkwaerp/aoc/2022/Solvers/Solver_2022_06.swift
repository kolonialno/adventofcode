//
//  Solver_2022_06.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 06/12/2022.
//

import Foundation

class Solver_2022_06: Solver {
    enum MarkerType {
        case start
        case message

        var markerLength: Int {
            switch self {
            case .start:
                return 4
            case .message:
                return 14
            }
        }
    }

    private var input: String = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString()
    }

    override func solveFunction1() -> String {
        let result = findMarkerIndex(in: input, type: .start)
        return "\(result)"
    }

    override func solveFunction2() -> String {
        let result = findMarkerIndex(in: input, type: .message)
        return "\(result)"
    }

    private func findMarkerIndex(in string: String, type: MarkerType) -> Int {
        let arrayed = string.convertToStringArray()

        let markerLength = type.markerLength
        var index = markerLength - 1

        while index < arrayed.count {
            let startIndex = index - markerLength + 1
            let set = Set(arrayed[startIndex...index])

            if set.count == markerLength {
                break
            }

            index += 1
        }


        return index + 1
    }
}

extension Solver_2022_06: TestableDay {
    func runTests() {
        let startIndices = ["bvwbjplbgvbhsrlpgdmjqwftvncz",
                       "nppdvjthqldpwncqszvftbrmjlhg",
                       "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
                       "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
        ].map { findMarkerIndex(in: $0, type: .start) }

        assert(startIndices == [5, 6, 10, 11])

        let messageIndices = ["mjqjpqmgbljsphdztnvjfqwrcgsmlb",
                              "bvwbjplbgvbhsrlpgdmjqwftvncz",
                              "nppdvjthqldpwncqszvftbrmjlhg",
                              "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
                              "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
        ].map { findMarkerIndex(in: $0, type: .message) }

        assert(messageIndices == [19, 23, 23, 29, 26])
    }
}
