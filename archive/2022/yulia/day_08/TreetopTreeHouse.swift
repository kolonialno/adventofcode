//
//  TreetopTreeHouse.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 08/12/2022.
//

import Foundation

struct TreetopTreeHouse {

    struct Point: Hashable {
        let x: Int
        let y: Int
    }

    func part1(_ input: String) -> Int {
        let lines = input.components(separatedBy: "\n")
            .map { [Character]($0).map { Int(String($0))! } }

        var visible = Set<Point>()

        for (y, line) in lines.enumerated() {
            for (x, tree) in line.enumerated() {
                guard !visible.contains(Point(x: x, y: y)) else { continue }
                guard y > 0 && x > 0 && y < lines.count - 1 && x < line.count - 1 else {
                    visible.insert(.init(x: x, y: y))
                    continue
                }

                let top = lines[0...y-1].map { $0[x] }
                let left = line[0...x-1]
                let right = line[x+1...line.count-1]
                let bottom = lines[y+1...lines.count-1].map { $0[x] }

                if top.contains(where: { $0 >= tree }) && bottom.contains(where: { $0 >= tree }) &&
                    left.contains(where: { $0 >= tree }) && right.contains(where: { $0 >= tree }) {
                    print("hidden: \(tree)")
                } else {
                    visible.insert(.init(x: x, y: y))
                }
            }
        }

        return visible.count
    }

    func part2(_ input: String) -> Int {
        let lines = input.components(separatedBy: "\n")
            .map { [Character]($0).map { Int(String($0))! } }

        var scores = Set<Int>()

        for (y, line) in lines.enumerated() {
            for (x, tree) in line.enumerated() {
                guard y > 0 && x > 0 && y < lines.count - 1 && x < line.count - 1 else {
                    continue
                }

                let top = lines[0...y-1].map { $0[x] }.reversed()
                let left = line[0...x-1].reversed()
                let right = line[x+1...line.count-1]
                let bottom = lines[y+1...lines.count-1].map { $0[x] }

                let topDistance = scenicScore(for: tree, list: Array(top))
                let leftDistance = scenicScore(for: tree, list: Array(left))
                let rightDistance = scenicScore(for: tree, list: Array(right))
                let bottomDistance = scenicScore(for: tree, list: bottom)
                scores.insert(bottomDistance * topDistance * leftDistance * rightDistance)
            }
        }

        return scores.max()!
    }

    private func scenicScore(for tree: Int, list: [Int]) -> Int {
        let distance: Int
        if let first = list.first(where: { $0 >= tree }) {
            distance = Array(list).firstIndex(of: first)! + 1
        } else {
            distance = list.count
        }

        return distance
    }
}
