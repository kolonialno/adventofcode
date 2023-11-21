//
//  DistressSignal.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 13/12/2022.
//

import Foundation

struct DistressSignal {

    func part1(_ input: String) -> Int {
        let pairItems = toItems(input)
        let pairsArrays = pairItems as! [[Any]]
        return pairsArrays.enumerated().compactMap { idx, pair -> Int? in
            if let result = compare(pair[0], pair[1]), result {
                return idx + 1
            } else {
                return nil
            }
        }
        .reduce(0, +)
    }

    func part2(_ input: String) -> Int {
        let pairItems = toItems(input)
        let pairsArrays = pairItems as! [[Any]]

        let toSort = pairsArrays.flatMap { $0 } + [[2]] + [[6]]
        let sorted = toSort.compactMap { $0 }.sorted { fst, snd in
            return compare(fst, snd) ?? true
        }

        let index = sorted.firstIndex(where: { ($0 as? [Int]) == [6] })!
        let index2 = sorted.firstIndex(where: { ($0 as? [Int]) == [2] })!
        return (index + 1) * (index2 + 1)
    }

    private func toItems(_ input: String) -> [Any] {
        let pairs = input.components(separatedBy: "\n\n")
        let pairsItems: [Any] = pairs.map { pair -> [Any] in
            let lines = pair.components(separatedBy: "\n")
            let items = lines.map { line -> [Any] in
                var stack: [Any] = []
                var currentArrayOfInts: [Any] = []
                var currentIntStr: String = ""

                for (idx, char) in line.enumerated() {
                    if char == "[" && idx > 0 {
                        stack.append(currentArrayOfInts)
                        currentArrayOfInts = []
                    } else if char == "]" {
                        if !currentIntStr.isEmpty {
                            let currentInt = Int(currentIntStr)!
                            currentArrayOfInts.append(currentInt)
                            currentIntStr = ""
                        }

                        if stack.isEmpty {
                            break
                        }

                        var last = !stack.isEmpty ? (stack.removeLast() as! [Any]) : []
                        last.append(currentArrayOfInts)
                        currentArrayOfInts = last
                    } else if char == "," {
                        if !currentIntStr.isEmpty {
                            let currentInt = Int(currentIntStr)!
                            currentArrayOfInts.append(currentInt)
                            currentIntStr = ""
                        }
                    } else if Int(String(char)) != nil {
                        currentIntStr.append(String(char))
                    }
                }

                return currentArrayOfInts
            }

            return items
        }

        return pairsItems
    }

    private func compare(_ fst: Any, _ snd: Any) -> Bool? {
        var result: Bool?
        if let firstInt = fst as? Int,
           let secondInt = snd as? Int {
            if firstInt < secondInt {
                result = true
            } else if firstInt > secondInt {
                result = false
            } else {
                result = nil
            }
        } else if let firstArray = fst as? [Any], let secondArray = snd as? [Any] {
            result = compareLists(firstArray, secondArray)

        } else if let fstAsInt = fst as? Int, let sndAsList = snd as? [Any] {
            let fstAsList = [fstAsInt]
            result = compareLists(fstAsList, sndAsList)

        } else if let fstAsList = fst as? [Any], let sndAsInt = snd as? Int {
            let sndAsList = [sndAsInt]
            result = compareLists(fstAsList, sndAsList)
        } else {
            fatalError("unexpected items")
        }

        return result
    }

    private func compareLists(_ fst: [Any], _ snd: [Any]) -> Bool? {
        var result: Bool?
        for (fstItem, sndItem) in zip(fst, snd) {
            result = compare(fstItem, sndItem)
            if result != nil {
                break
            }
        }

        if result == nil {
            if fst.count < snd.count {
                result = true
            } else if fst.count > snd.count {
                result = false
            }
        }

        return result
    }
}
