//
//  Solver_2022_20.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 20/12/2022.
//

import Foundation

class Solver_2022_20: Solver {
    class Mixer {
        struct Entry {
            let value: Int
            let index: Int
        }

        let solver: Solver?
        let originalList: [Entry]
        var currentList: [Entry]

        init(list: [Int], decryptionKey: Int = 1, solver: Solver? = nil) {
            self.originalList = list.enumerated().map { Entry(value: $0.element * decryptionKey, index: $0.offset) }
            self.currentList = originalList
            self.solver = solver
        }

        func mix(times: Int) -> Int {
            let listLength = originalList.count - 1
            (0..<times).forEach { t in
                let text = "\(t) / \(times) (\((t * 100) / times) %)"
                solver?.visualizeCurrentPart(text: text)

                for original in originalList {
                    guard original.value != 0 else { continue }
                    guard let currentIndex = currentList.firstIndex(where: { $0.index == original.index }) else { fatalError() }

                    var nextIndex = (currentIndex + original.value) % listLength
                    if nextIndex < 0 {
                        nextIndex +=  listLength
                    }

                    let removed = currentList.remove(at: currentIndex)
                    currentList.insert(removed, at: nextIndex)
                }
            }
            solver?.visualizeCurrentPart(text: "100 %")

            guard let indexOfZero = currentList.firstIndex(where: { $0.value == 0 }) else { fatalError() }

            return [1000, 2000, 3000]
                .map { (indexOfZero + $0) % currentList.count }
                .map { currentList[$0].value }
                .reduce(0, +)
        }
    }

    private var decryptionKey = 811589153

    override func solveFunction1() -> CustomStringConvertible {
        let input = defaultInputFileString.loadAsStringArray().map { Int($0)! }
        let mixer = Mixer(list: input, solver: self)
        let coordinateSum = mixer.mix(times: 1)
        return "\(coordinateSum)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let input = defaultInputFileString.loadAsStringArray().map { Int($0)! }
        let mixer = Mixer(list: input, decryptionKey: decryptionKey, solver: self)
        let coordinateSum = mixer.mix(times: 10)
        return "\(coordinateSum)"
    }
}
