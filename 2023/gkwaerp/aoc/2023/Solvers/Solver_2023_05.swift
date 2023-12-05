//
//  Solver_2023_05.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 05/12/2023.
//

import Foundation

final class Solver_2023_05: Solver {
    private var input = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString()
    }

    override func solveFunction1() -> CustomStringConvertible {
        Almanac(input, usingRanges: false).findLowestLocation()
    }

    override func solveFunction2() -> CustomStringConvertible {
        Almanac(input, usingRanges: true).findLowestLocation()
    }
}

extension Solver_2023_05: TestableDay {
    func runTests() {
        let input = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

        let almanac1 = Almanac(input, usingRanges: false)
        let result1 = almanac1.findLowestLocation()
        let expected1 = 35
        assert(result1 == expected1)

        let almanac2 = Almanac(input, usingRanges: true)
        let result2 = almanac2.findLowestLocation()
        let expected2 = 46
        assert(result2 == expected2)
    }
}

extension Solver_2023_05 {
    struct Almanac {
        let seeds: [Int]
        let seedRanges: [Range<Int>]
        let maps: [Map]
        let usingRanges: Bool

        init(_ string: String, usingRanges: Bool) {
            let strings = string.components(separatedBy: "\n\n")

            let seeds = strings[0]
                .components(separatedBy: .whitespaces)
                .compactMap { $0.intValue }

            let seedRanges = stride(from: 0, to: seeds.count, by: 2)
                .map {
                    let start = seeds[$0]
                    let end = seeds[$0] + seeds[$0 + 1]
                    return start..<end
                }

            self.seeds = seeds
            self.seedRanges = seedRanges
            self.maps = strings[1...].map { Map($0) }
            self.usingRanges = usingRanges
        }

        func findLowestLocation() -> Int {
            if usingRanges {
                return seedRanges.reduce(Int.max, { partialResult, range in
                    min(partialResult, range.reduce(Int.max, { min($0, getLocation(for: $1)) }))
                })
            } else {
                return seeds.reduce(Int.max, { min($0, getLocation(for: $1)) })
            }
        }

        private func getLocation(for seed: Int) -> Int {
            maps.reduce(seed, { $1.convert(number: $0) })
        }
    }
}

extension Solver_2023_05.Almanac {
    struct Map {
        let id: String
        let mapLines: [MapLine]

        init(_ string: String) {
            let strings = string
                .components(separatedBy: .newlines)
                .filter { !$0.isEmpty }
            self.id = strings[0]
            self.mapLines = strings[1...].map { MapLine($0) }
        }

        func convert(number: Int) -> Int {
            for mapLine in mapLines {
                if let mapped = mapLine.convert(number: number) {
                    return mapped
                }
            }

            return number
        }
    }
}

extension Solver_2023_05.Almanac.Map {
    struct MapLine {
        let destination: Int
        let source: Int
        let rangeLength: Int

        let sourceRange: Range<Int>

        init(_ string: String) {
            let values = string
                .components(separatedBy: .whitespaces)
                .compactMap { $0.intValue }
            self.destination = values[0]
            self.source = values[1]
            self.rangeLength = values[2]

            self.sourceRange = source..<(source + rangeLength)
        }

        func convert(number: Int) -> Int? {
            sourceRange.contains(number) ? number - source + destination : nil
        }
    }
}
