//
//  BeaconExclusionZone.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 15/12/2022.
//

import Foundation

struct BeaconExclusionZone {

    struct Position: Hashable {
        let x: Int
        let y: Int
    }

    func part1(_ input: String, y: Int) -> Int {
        let map = sensorsAndBeacons(input)
        let ranges = ranges(from: map, y: y)
        let beaconsInRowCount = Set(map.values.filter { $0.y == y && ranges[0].contains($0.x) }).count

        return ranges[0].count - beaconsInRowCount
    }

    func part2(_ input: String, maxy: Int) -> Int {
        let map = sensorsAndBeacons(input)
        for y in Array(0...maxy) {
            let ranges = ranges(from: map, y: y)
            if ranges.count > 1 {
                return (ranges[0].upperBound + 1) * 4000000 + y
            }
        }

        return 0
    }

    private func ranges(from map: [Position: Position], y: Int) -> [ClosedRange<Int>] {
        let ranges = map.keys.compactMap { sensor -> ClosedRange<Int>? in
            let distance = distance(from: sensor, to: map[sensor]!)
            let rxs = sensor.x - (distance - abs(y - sensor.y))
            let rxe = sensor.x + (distance - abs(y - sensor.y))
            guard rxs <= rxe else { return nil }
            return rxs...rxe
        }

        var sorted = ranges.sorted(by: { range1, range2 -> Bool in
            if range1.lowerBound == range2.lowerBound {
                return range1.upperBound < range2.upperBound
            } else {
                return range1.lowerBound < range2.lowerBound
            }
        })

        var first = sorted.removeFirst()
        var rangesMerged = [ClosedRange<Int>]()
        for range in sorted {
            if first.overlaps(range) {
                let ends = [first.upperBound, range.upperBound]
                first = first.lowerBound...ends.max()!
            } else {
                rangesMerged.append(first)
                first = range
            }
        }

        rangesMerged.append(first)
        return rangesMerged
    }

    private func distance(from: Position, to: Position) -> Int {
        return abs(from.x - to.x) + abs(from.y - to.y)
    }

    private func sensorsAndBeacons(_ input: String) -> [Position: Position] {
        let lines = input.components(separatedBy: "\n")

        var map: [Position: Position] = [:]
        for line in lines {
            let comp = line.components(separatedBy: ":")
            let fsts = comp[0].components(separatedBy: ", ")
            let sx = Int(fsts[0].components(separatedBy: "x=")[1])!
            let sy = Int(fsts[1].components(separatedBy: "y=")[1])!
            let fstb = comp[1].components(separatedBy: ", ")
            let bx = Int(fstb[0].components(separatedBy: "x=")[1])!
            let by = Int(fstb[1].components(separatedBy: "y=")[1])!

            map[.init(x: sx, y: sy)] = .init(x: bx, y: by)
        }

        return map
    }
}
