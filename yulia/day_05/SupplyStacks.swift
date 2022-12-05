//
//  SupplyStacks.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 05/12/2022.
//

import Foundation

struct SupplyStacks {

    func part1(_ input: String) -> String {
        var stacksInfo = stackInfo(from: input)
        let components = input.components(separatedBy: "\n\n")
        let movesComponents = components[1]
        let moves = movesComponents.components(separatedBy: "\n")

        for move in moves {
            let comp = move.components(separatedBy: " from ")
            let boxes = Int(comp[0].components(separatedBy: " ")[1])!
            let from = Int(comp[1].components(separatedBy: " to ")[0])!
            let to = Int(comp[1].components(separatedBy: " to ")[1])!

            for _ in Array(1...boxes) {
                var fromArray = stacksInfo[from]!
                let last = fromArray.last!
                var toArray = stacksInfo[to]!
                toArray.append(last)
                fromArray = fromArray.dropLast(1)

                stacksInfo[from] = fromArray
                stacksInfo[to] = toArray
            }
        }

        let all = stacksInfo.sorted(by: { $0.key < $1.key }).map { $0.value.last! }
        return all.joined().replacingOccurrences(of: "]", with: "").replacingOccurrences(of: "[", with: "")
    }

    func part2(_ input: String) -> String {
        var stacksInfo = stackInfo(from: input)
        let components = input.components(separatedBy: "\n\n")
        let movesComponents = components[1]
        let moves = movesComponents.components(separatedBy: "\n")

        for move in moves {
            let comp = move.components(separatedBy: " from ")
            let boxes = Int(comp[0].components(separatedBy: " ")[1])!
            let from = Int(comp[1].components(separatedBy: " to ")[0])!
            let to = Int(comp[1].components(separatedBy: " to ")[1])!

            var fromArray = stacksInfo[from]!
            let boxesToMove = fromArray.suffix(boxes)
            var toArray = stacksInfo[to]!
            toArray.append(contentsOf: boxesToMove)
            fromArray = fromArray.dropLast(boxes)

            stacksInfo[from] = fromArray
            stacksInfo[to] = toArray
        }

        let all = stacksInfo.sorted(by: { $0.key < $1.key }).map { $0.value.last! }
        return all.joined().replacingOccurrences(of: "]", with: "").replacingOccurrences(of: "[", with: "")
    }

    private func stackInfo(from input: String) -> [Int: [String]] {
        let components = input.components(separatedBy: "\n\n")
        let stacksComponents = components[0]
        let movesComponents = components[1]
        let stacks = stacksComponents.components(separatedBy: "\n").reversed()

        var stacksInfo = [Int: [String]]()

        for stack in stacks {
            let stackArray = [Character](stack)
            let strided = stride(from: 0, to: stackArray.count, by: 4).enumerated().map { idx, stride -> (Int, String) in
                let array = Array(stackArray[stride..<min(stride + 3, stackArray.count)])
                let string = array.map { String($0) }.joined().trimmingCharacters(in: .whitespaces)
                return (idx, string)
            }

            print(strided)

            if strided.contains(where: { $0.1.contains("[") }) {
                strided.forEach {
                    var array: [String]
                    if let existingArray = stacksInfo[$0.0 + 1] {
                        array = existingArray
                    } else {
                        array = [String]()
                    }

                    array.append($0.1)
                    stacksInfo[$0.0 + 1] = array.filter { !$0.isEmpty }
                }
            }
        }
        print(stacksInfo)
        return stacksInfo
    }
}
